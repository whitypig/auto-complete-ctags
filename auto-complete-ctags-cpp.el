;;; auto-complete-ctags-cpp.el --- 

;; Copyright (C) 2013  whitypig

;; Author: whitypig <whitypig@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'auto-complete-ctags)
(require 'cl)

(defun ac-ctags-cpp-line-has-typeinfo-p (varname line)
  "Return t if this LINE contains type name, or nil."
  (let ((case-fold-search nil)
        (type-regexp1 (concat
                      "\\([[:alnum:]<>_, ]+::\\)+" ; namespace
                      "[[:alnum:]<>_, ]+" ; classname
                      "[[:space:]]*[&*]*[[:space:]]*[&*]*"))
        (type-regexp2 (concat
                       "\\([[:alnum:]<>_]+\\)" ; classname
                       "[[:space:]]*[^;\n]+[&*]*[[:space:]]*[&*]*"))
        (exclude-regexp "return"))
    (and (or (string-match-p (concat type-regexp1
                                     ;"[[:space:]*]+"
                                     varname
                                     "[,=;)[:space:]]"
                                     )
                             line)
             (string-match-p (concat type-regexp2
                                     varname
                                     "[,=;)[:space:]]")
                             line)
             (string-match-p (concat varname
                                     "[ =]+"
                                     "new")
                             line))
         (not (string-match-p exclude-regexp line))
         (not (string-match-p "^[[:space:]]*//" line)))))

(defun ac-ctags-cpp-determine-type-name ()
  "Retrun a typename if possible, nil otherwise."
  ;; ac-prefix for method candidates is '\\.\\(.*\\)' or '\\->\\(.*\\)'
  (let* ((case-fold-search nil)
         (end (save-excursion (or (save-excursion
                                    (re-search-backward "\\." (line-beginning-position) t 1))
                                  (save-excursion
                                    (re-search-backward "->" (line-beginning-position) t 1)))))
         (beg (save-excursion (and end
                                   (goto-char end)
                                   (or (and (re-search-backward "[;{}=]" (line-beginning-position) t 1)
                                            (1+ (point)))
                                       (line-beginning-position)))))
         (str-before-dot
          (and (integerp beg) (integerp end) (< beg end)
               (ac-ctags-trim-whitespace
                (buffer-substring-no-properties beg end)))))
    (when (stringp str-before-dot)
      (ac-ctags-cpp-determine-type-name-1 str-before-dot))))

(defun ac-ctags-cpp-current-class-name ()
  "Return a class name where the current context is in, or nil."
  ;; TODO
  ;; cpp allows codes that are NOT in a class
  (ac-ctags-java-current-type-name))

(defun ac-ctags-cpp-determine-type-name-1 (str-before-dot)
  (assert (stringp str-before-dot) t "ac-ctags-cpp-determine-type-name-1")
  (cond
   ((string= "this" str-before-dot)
    ;; return this class name
    (ac-ctags-cpp-current-class-name))
   ((string-match "^[A-Z][A-Za-z_0-9]+$" str-before-dot)
    ;; this is probably a classname, so we return it as is.
    str-before-dot)
   ((string-match "^[_a-z][A-Za-z_0-9]+$" str-before-dot)
    ;; str-before-dot seems a valid variable name
    (ac-ctags-cpp-strip-angle-brackets
     (ac-ctags-cpp-get-typename-of-variable str-before-dot)))
   (t
    (let ((identifier (ac-ctags-cpp-parse-before-dot-or-arrow-part str-before-dot)))
      (when (stringp identifier)
        (cond
         ((string-match (concat identifier "(")
                        str-before-dot)
          ;; this is a function name
          ;; If str-before-dot is like SomeClass::static_function(),
          ;; identifier should be SomeClass::static_function.
          (ac-ctags-cpp-get-function-return-type identifier))
         (t
          ;; this is a variable name
          (ac-ctags-cpp-get-typename-of-variable identifier))))))))

(defun ac-ctags-cpp-strip-angle-brackets (string)
  (when (stringp string)
    (replace-regexp-in-string "<.*>" "" string)))

(defun ac-ctags-cpp-get-typename-of-variable (varname)
  (or (ac-ctags-cpp-extract-type-name
       (ac-ctags-cpp-extract-variable-line varname) varname)
      (ac-ctags-cpp-get-typename-of-variable-1 varname)))

(defun ac-ctags-cpp-get-typename-of-variable-1 (varname)
  ;; we have to go through tags file
  (loop with case-fold-search = nil
        for node in (ac-ctags-get-lang-db "C++")
        for name = (ac-ctags-node-name node)
        for kind = (ac-ctags-node-kind node)
        for cmd = (ac-ctags-node-command node)
        when (and (stringp kind)
                  (stringp cmd)
                  (string= name varname))
        do (return (ac-ctags-cpp-extract-type-name cmd varname))))

(defun ac-ctags-cpp-extract-type-name (line varname)
  "Extract a type of VARNAME from LINE and return it if found, or nil.
Also we ignore primitive types such as int, double."
  (when (stringp line)
    (if (string-match (concat varname "[ ]*=[ ]*new[ ]+\\([^;()<> ]+\\)") line)
        ;; case for "varname = new Class;"
        (match-string-no-properties 1 line)
      (loop with case-fold-search = nil
            for ch in (nreverse (split-string
                                 (ac-ctags-cpp-extract-type-name-1 line varname)
                                 ""
                                 t))
            with acc = nil
            with stack = nil
            with nparen = 0
            with nangle-bracket = 0
            if (string= ch ">")
            do (progn (incf nangle-bracket)
                      (push ch acc))
            else if (string= ch "<")
            do (progn (decf nangle-bracket)
                      (push ch acc))
            else if (string= ch "-")
            do (when (and (stringp (car acc)) (string= (car acc) ">"))
                 ;; the previous angle bracket is for arrow operator
                 (decf nangle-bracket)
                 (push ch acc))
            else if (or (string= ch " ") (string= ch ","))
            do (cond
                ((and (zerop nparen) (zerop nangle-bracket))
                 (when acc (push (reduce #'concat acc) stack))
                 (setq acc nil))
                (t
                 (push ch acc)))
            else if (not (member ch '("(" ")")))
            do (push ch acc)
            finally (return (progn (when acc
                                     (push (reduce #'concat acc) stack))
                                   (ac-ctags-cpp-strip-aster-and-amp
                                    (car (ac-ctags-cpp-remove-keyword stack)))))))))

(defun ac-ctags-cpp-extract-type-name-1 (line varname)
  "Return neccessary part of string in LINE to determine type of VARNAME."
  (let ((ln (ac-ctags-trim-whitespace line)))
    (cond
     ((string-match (concat "(""\\([^()]+" varname "\\))") line)
      (match-string-no-properties 1 line))
     (t
      line))))

(defun ac-ctags-cpp-strip-aster-and-amp (s)
  "Remove leading and trailinig asterisks or ampersands"
  (replace-regexp-in-string "[*&]+$"
                            ""
                            (replace-regexp-in-string "^[*&]+" "" s)))


(defun ac-ctags-cpp-extract-variable-line (varname)
  "Return string which we has inferred has a typename of VARNAME."
  (or (ac-ctags-cpp-extract-variable-line-1 varname
                                             (point-min)
                                             (point))
      (ac-ctags-cpp-extract-variable-line-1 varname
                                             (1+ (point))
                                             (point-max))))

(defun ac-ctags-cpp-extract-variable-line-1 (varname beg end)
  (save-excursion
    (loop with case-fold-search = nil
          while (re-search-forward (concat "[ \t*]" varname "[; =)]")
                                   end
                                   t)
          initially do (goto-char beg)
          when (ac-ctags-cpp-line-has-typeinfo-p
                varname
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position)))
          return (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position)))))

(defun ac-ctags-cpp-get-function-return-type (identifier)
  (loop with case-fold-search = nil
        for node in (ac-ctags-get-lang-db "C++")
        for name = (ac-ctags-node-name node)
        for kind = (ac-ctags-node-kind node)
        for returntype = (ac-ctags-node-returntype node)
        when (and (stringp kind)
                  (stringp returntype)
                  (string= name identifier)
                  (or (string= kind "function")
                      (string= kind "prototype")))
        do (return returntype)))

(defun ac-ctags-cpp-parse-before-dot-or-arrow-part (string)
  "Parse part before dot or arrow and return the identifier if possible."
  (loop named this-func
        with case-fold-search = nil
        with stack = nil
        with paren-count = 0
        with identifier = nil
        for ch in (nreverse (split-string string "" t))
        if (string= ch ")")
        do (incf paren-count)
        else if (string= ch "(")
        do (cond
            ((zerop paren-count)
             (return-from this-func (and identifier
                                         (ac-ctags-trim-whitespace
                                          (reduce #'concat identifier)))))
            (t
             (decf paren-count)
             (when identifier
               (push (ac-ctags-trim-whitespace (reduce #'concat identifier))
                     stack))
             (setq identifier nil)))
        else if (or (string= ch " ") (string= ch "."))
        do (cond
            ((zerop paren-count)
             ;; this space or dot delimits this expression
             (return-from this-func
               (and identifier
                    (ac-ctags-trim-whitespace
                     (reduce #'concat identifier)))))
            (t
             (push ch identifier)))
        else if (string-match "[a-zA-Z0-9_:]" ch)
        do (push ch identifier)
        finally (return-from this-func
                  (progn
                    (when identifier
                      (push (ac-ctags-trim-whitespace (reduce #'concat identifier))
                            stack))
                    (car (ac-ctags-cpp-remove-keyword stack))))))

(defun ac-ctags-cpp-remove-keyword (lst)
  (let ((case-fold-search nil))
    (remove-if (lambda (s)
                 (string-match (concat "^\\("
                                       "const\\|"
                                       "return\\|"
                                       "for\\|"
                                       "if\\|"
                                       "\\)$")
                               s))
               lst)))

(defun ac-ctags-cpp-collect-member-functions (classname prefix)
  (loop with case-fold-search = nil
        for node in (ac-ctags-get-lang-db "C++")
        for name = (ac-ctags-node-name node)
        for kind = (ac-ctags-node-kind node)
        for class = (ac-ctags-node-class node)
        when (and (stringp classname)
                  (stringp kind)
                  (stringp class)
                  (string= class classname)
                  (or (string= kind "function") (string= kind "prototype"))
                  ;; exclude names having a scope operator
                  (not (string-match-p "::" name))
                  (or (null prefix)
                      (string= prefix "")
                      (string-match-p (concat "^" prefix "[^:]*") name)))
        collect (ac-ctags-cpp-make-function-candidate node)))

(defun ac-ctags-cpp-member-function-candidates ()
  (let ((type (ac-ctags-cpp-determine-type-name)))
    (message "DEBUG: type=%s" type)
    (ac-ctags-cpp-member-function-candidates-1 type ac-prefix)))

(defun ac-ctags-cpp-member-function-candidates-1 (classname prefix)
  "Return candidates that begin with PREFIX and that are member
functions of class CLASSNAME."
  (when (stringp classname)
    ;; I don't care whether CLASSNAME is a pointer or not.
    ;; I just treat it as a class, so remove "*" from CLASSNAME.
    (let ((typename (ac-ctags-cpp-strip-typename classname)))
      (or (ac-ctags-cpp-collect-member-functions typename prefix)
          ;; If no candidates are found, strip namespace and do the job.
          (when (string-match "::\\([^:]+\\)" typename)
            (ac-ctags-cpp-collect-member-functions
             (match-string-no-properties 1 typename) prefix))))))

(defun ac-ctags-cpp-strip-typename (typename)
  "Remove c++ keyowrds, template arguments, and \"*\" from TYPENAME."
  (assert (stringp typename) "ac-ctags-cpp-strip-typename")
  (let ((ret typename))
    (setq ret (replace-regexp-in-string "\\(const\\)"
                                        ""
                                        ret))
    (setq ret (replace-regexp-in-string "<.*>" "" ret))
    (setq ret (replace-regexp-in-string "[ *]+$"
                                        ""
                                        ret))
    (ac-ctags-trim-whitespace ret)))

(defun ac-ctags-cpp-member-function-prefix ()
  (let ((ch1 (char-before))
        (ch2 (char-before (1- (point)))))
    (when (characterp ch1)
      (cond
       ((char-equal ch1 ?.)
        (point))
       ((char-equal ch1 ?>)
        (when (and (characterp ch2)
                   (char-equal ch2 ?-))
          ;; ->
          (point)))
       (t
        (when (save-excursion
                (re-search-backward "\\(\\.\\|->\\)" nil (line-beginning-position)))
          (match-end 1)))))))

(defun ac-ctags-cpp-get-members-by-scope-operator (class prefix)
  "Return a list of strings that begin with PREFIX and that are
members in CLASS. CLASS is either classname or namespace. If
PREFIX is nil or empty string, return all members of CLASS."
  (loop with case-fold-search = nil
        with needle = (concat class "::" prefix)
        for node in (ac-ctags-get-lang-db "C++")
        for name = (ac-ctags-node-name node)
        for kind = (ac-ctags-node-kind node)
        for classname = (ac-ctags-node-class node)
        for namespace = (ac-ctags-node-namespace node)
        with nlimits = ac-ctags-candidate-limit
        with candidates = nil
        with count = 0
        when (and (or
                   ;; case for "::foo"
                   (or (null class) (string= "" class))
                   ;; check classname
                   (and (stringp classname)
                        (string= classname class)
                        (not (string-match-p "::" name)))
                   ;; check namespace
                   (and (stringp namespace)
                        (string-match (concat class "$") namespace)
                        (not (string-match-p "::" name))))
                  (or (null prefix)
                      (string= prefix "")
                      (string-match (concat "^" prefix) name)))
        do (progn (push (ac-ctags-cpp-make-candidate node) candidates)
                  (incf count)
                  (when (>= count nlimits)
                    (return (sort candidates #'string<))))
        finally (return (sort candidates #'string<))))

(defun ac-ctags-cpp-make-candidate (node)
  (let ((kind (ac-ctags-node-kind node))
        (case-fold-search nil))
    (cond
     ((null kind)
      nil)
     ((member kind '("function" "prototype"))
      (ac-ctags-cpp-make-function-candidate node))
     ((string= kind "member")
      (ac-ctags-cpp-make-field-candidate node))
     (t
      ;; for now we return this node's name as is.
      (ac-ctags-node-name node)))))

(defun ac-ctags-cpp-make-function-candidate (node)
  "Return presentation form of NODE.
Signature property is used to construct yasnippet template."
  (let* ((ret (concat (ac-ctags-node-name node)
                      (ac-ctags-cpp-remove-trailing-keyword-from-signature
                       (ac-ctags-node-signature node))))
         (returntype (when (ac-ctags-node-returntype node)
                       (concat ":" (ac-ctags-node-returntype node))))
         (classname (or (ac-ctags-node-class node) (ac-ctags-node-interface node)))
         (viewprop (concat returntype " - " classname)))
    (propertize ret
                'view (concat ret
                              (ac-ctags-get-spaces-to-insert ret viewprop)
                              returntype
                              " - " classname)
                'signature (ac-ctags-cpp-remove-trailing-keyword-from-signature
                            (ac-ctags-node-signature node)))))

(defun ac-ctags-cpp-remove-trailing-keyword-from-signature (signature)
  (replace-regexp-in-string ".*)\\([^()]+\\)$" "" signature nil nil 1))

(defun ac-ctags-cpp-make-field-candidate (node)
  (ac-ctags-java-make-field-candidate node))

(defun ac-ctags-cpp-split-string-by-separator (string separator)
  "Split STRING by SEPARATOR which could be regexp.
For example, std::vector<int>:: => (\"std\" \"::\" \"vector<int>\" \"::\")"
  (let ((ret (reduce (lambda (x y)
                       (cons x
                             (cons separator y)))
                     (split-string string (regexp-quote separator) t)
                     :initial-value nil
                     :from-end t)))
    (if (string-match (concat "" separator "$") string)
        ret
      (nbutlast ret))))

(defun ac-ctags-cpp-scope-member-candidates ()
  (ac-ctags-cpp-get-members-by-scope-operator
   (ac-ctags-cpp-parse-before-scope-operator)
   ac-prefix))

(defun ac-ctags-cpp-parse-before-scope-operator ()
  (ac-ctags-trim-whitespace
   (ac-ctags-cpp-parse-before-scope-operator-1
    (buffer-substring-no-properties
     (save-excursion
       (or (re-search-backward "[;{}()=]" (line-beginning-position) t)
           (line-beginning-position)))
     (save-excursion
       ;; should match
       (re-search-backward "::" (line-beginning-position) t)
       (match-beginning 0))))))

(defun ac-ctags-cpp-parse-before-scope-operator-1 (string)
  (loop named this-func
        with case-fold-search = nil
        with lst = (nreverse
                    (split-string
                     (reduce #'concat (ac-ctags-cpp-remove-keyword
                                       (split-string string " " t)))
                     ""
                     t))
        with len = (length lst)
        for ch in lst
        for i from 0
        with acc = nil
        with nangle-bracket = 0
        if (string= ch ">")
        do (incf nangle-bracket)
        else if (string= ch "<")
        do (decf nangle-bracket)
        else if (string= ch ":")
        do (when (and (< (1+ i) len)
                      (string= ":" (nth (1+ i) lst))
                      (zerop nangle-bracket))
             (return-from this-func (reduce #'concat acc)))
        else
        do (when (zerop nangle-bracket)
             (push ch acc))
        finally (return-from this-func (reduce #'concat acc))))

(defun ac-ctags-cpp-function-action ()
  "Expand yasnippet template for this method signature."
  (when (fboundp 'yas-expand-snippet)
    (let* ((cand (cdr ac-last-completion))
           (signature (get-text-property 0 'signature cand))
           (template (and (stringp signature)
                          (ac-ctags-make-yasnippet-template-from-signature
                           signature))))
      (when (stringp template)
        (delete-char (- (length signature)))
        (yas-expand-snippet template)))))

(defun ac-ctags-cpp-macro-candidates ()
  (ac-ctags-cpp-macro-candidates-1 ac-prefix))

(defun ac-ctags-cpp-macro-candidates-1 (prefix)
  "Return candidates as a list of strings which begin with PREFIX."
  (loop with case-fold-search = nil
        with nlimits = ac-ctags-candidate-limit
        for node in (ac-ctags-get-lang-db "C++")
        for name = (ac-ctags-node-name node)
        for kind = (ac-ctags-node-kind node)
        with candidates = nil
        when (and (stringp kind)
                  (string= kind "macro")
                  (string-match (concat "^" prefix)
                                name))
        collect name into candidates
        finally (return (sort (if (> (length candidates) nlimits)
                                  (nbutlast candidates nlimits)
                                candidates)
                              #'string<))))

;; Definitions of each ac-source for cpp
(ac-define-source ctags-cpp-member-functions
  '((candidates . ac-ctags-cpp-member-function-candidates)
    ;; commented out for debug purpose
    ;(cache)
    (candidate-face . ac-ctags-candidate-face)
    (selection-face . ac-ctags-selection-face)
    (requires . 0)
    ;; prefix is either "." or "->"
    (prefix . ac-ctags-cpp-member-function-prefix)
    (action . ac-ctags-cpp-function-action)))

(ac-define-source ctags-cpp-scope-members
  '((candidates . ac-ctags-cpp-scope-member-candidates)
    ;; commented out for debug purpose
    ;(cache)
    (candidate-face . ac-ctags-candidate-face)
    (selection-face . ac-ctags-selection-face)
    (requires . 0)
    (prefix . "::\\(.*\\)")
    (action . ac-ctags-cpp-function-action)
    ))

(ac-define-source ctags-cpp-macro
  '((candidates . ac-ctags-cpp-macro-candidates)
    (candidate-face . ac-ctags-candidate-face)
    (selection-face . ac-ctags-selection-face)
    (requires . 2)))

(defun ac-ctags-cpp-add-sources ()
  (interactive)
  (add-to-list 'ac-sources 'ac-source-ctags-cpp-member-functions)
  (add-to-list 'ac-sources 'ac-source-ctags-cpp-scope-members)
  (add-to-list 'ac-sources 'ac-source-ctags-cpp-macro))

(provide 'auto-complete-ctags-cpp)
;;; auto-complete-ctags-cpp.el ends here