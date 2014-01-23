;;; auto-complete-ctags-java.el --- 

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
(require 'cl)
(require 'auto-complete-ctags)

;; ac-prefix for method candidates is '\\.\\(.*\\)'
(defun ac-ctags-java-method-candidates ()
  "Candidates function of `ac-source-ctags-java-method'."
  ;;(message "DEBUG: ac-ctags-java-method-candidates, ac-prefix=%s" ac-prefix)
  (ac-ctags-java-method-candidates-1
   (ac-ctags-java-determine-class-name) ac-prefix))

(defun ac-ctags-java-method-candidates-1 (classname prefix)
  "Return method candidates which belong to class CLASSNAME and
whose name begins with PREFIX. If PREFIX is nil, return all
methods in CLASSNAME. If CLASSNAME is nil, return nil."
  ;;(message "DEBUG: method-candidates-1, classname=%s, prefix=%s" classname prefix)
  (cond
   ((null classname) nil)
   ((or (null prefix) (not (stringp prefix)) (string= prefix ""))
    (ac-ctags-java-collect-methods-in-class classname ""))
   (t
    (ac-ctags-java-collect-methods-in-class classname prefix))))

(defun ac-ctags-java-collect-methods-in-class (classname prefix)
  "Return a list of method names which belong to CLASSNAME."
  (loop for node in (ac-ctags-get-nodes-by-lang-and-name "Java" prefix)
        with ret = nil
        with case-fold-search = nil
        for kind = (ac-ctags-node-kind node)
        for class = (or (ac-ctags-node-class node) (ac-ctags-node-interface node))
        when (and (stringp class)
                  (stringp kind)
                  (string= "method" kind)
                  (or (string-match (concat "^" classname "$") class)
                      ;; To include OuterClass.InnerClass notation
                      (string-match (concat "\\." classname "$") class)))
        do (push (ac-ctags-java-make-method-candidate node) ret)
        finally (return (sort ret #'string<))))

(defun ac-ctags-java-method-action ()
  "Expand yasnippet template for this method signature."
  (when (fboundp 'yas-expand-snippet)
    (let* ((cand (cdr ac-last-completion))
           (signature (get-text-property 0 'signature cand))
           (template (and (stringp signature)
                          (ac-ctags-make-yasnippet-template-from-signature signature))))
      (when (stringp template)
        (delete-char (- (length signature)))
        (yas-expand-snippet template)))))

(defun ac-ctags-java-field-candidates ()
  "Candidate function for completing field names."
  (ac-ctags-java-field-candidates-1
   (ac-ctags-java-determine-class-name) ac-prefix))

(defun ac-ctags-java-field-candidates-1 (classname prefix)
  "return a list of field names which belong to CLASSNAME and
which begin with PREFIX."
  (cond
   ((null classname) nil)
   ((or (null prefix) (not (stringp prefix)) (zerop (length prefix)))
    (ac-ctags-java-collect-fields-in-class classname ""))
   (t
    (ac-ctags-java-collect-fields-in-class classname prefix))))

(defun ac-ctags-java-collect-fields-in-class (classname prefix)
  "Return a list of fields in class/interface CLASSNAME."
  (loop with ret = nil
        with case-fold-search = nil
        for node in (ac-ctags-get-nodes-by-lang-and-name "Java" prefix)
        for kind = (ac-ctags-node-kind node)
        for class = (or (ac-ctags-node-class node) (ac-ctags-node-interface node))
        when (and (stringp class)
                  (stringp kind)
                  (string= "field" kind)
                  (or (string-match (concat "^" classname "$") class)
                      ;; To include OuterClass.InnerClass notation
                      (string-match (concat "\\." classname "$") class)))
        do (push (ac-ctags-java-make-field-candidate node) ret)
        finally (return (sort ret #'string<))))

(defun ac-ctags-java-make-field-candidate (node)
  "Return a propertized filed name."
  (let ((ret (ac-ctags-node-name node))
        (type (ac-ctags-java-parse-field-node node)))
    (if (not (stringp type))
        ret
      (propertize ret
                  'view (concat ret
                                (ac-ctags-get-spaces-to-insert ret (concat " :" type))
                                " :" type)))))

(defun ac-ctags-java-parse-field-node (node)
  "Return a type name of this field by parsing NODE list if possible.
If field declaration spans to multiple lines, this function
cannot parse that field, and return nil."
  (let* ((case-fold-search nil)
         (name (ac-ctags-node-name node))
         (cmd (ac-ctags-node-command node))
         (splitted (split-string cmd "[ \t;]"))
         (end (position name splitted :test #'string=))
         (start (position-if (lambda (str)
                               (string-match (concat "private\\|"
                                                     "protected\\|"
                                                     "public\\|"
                                                     "final\\|"
                                                     "transient\\|"
                                                     "static\\|"
                                                     "volatile")
                                             str))
                             splitted :from-end t)))
    (when (and start end (< (1+ start) end))
      (reduce (lambda (x y)
                (concat x " " y))
              (subseq splitted (1+ start) end)))))


;; method :returntype - class
(defun ac-ctags-java-make-method-candidate (node)
  "Return presentation form of NODE.
Signature property is used to construct yasnippet template."
  (let* ((ret (concat (ac-ctags-node-name node)
                      (ac-ctags-node-signature node)))
         (returntype (when (ac-ctags-node-returntype node)
                       (concat ":" (ac-ctags-node-returntype node))))
         (classname (or (ac-ctags-node-class node) (ac-ctags-node-interface node)))
         (viewprop (concat returntype " - " classname)))
    (propertize ret
                'view (concat ret
                              (ac-ctags-get-spaces-to-insert ret viewprop)
                              returntype
                              " - " classname)
                'signature (ac-ctags-node-signature node))))

(defun ac-ctags-java-determine-class-name ()
  "Retrun a classname if possible, nil otherwise."
  ;; ac-prefix for method candidates is '\\.\\(.*\\)'
  (let* ((case-fold-search nil)
         (end (save-excursion (re-search-backward "\\." (line-beginning-position) t 1)))
         (beg (save-excursion (and end
                                   (goto-char end)
                                   (or (and (re-search-backward "[;{}]" (line-beginning-position) t 1)
                                            (1+ (point)))
                                       (line-beginning-position)))))
         (str-before-dot
          (and (integerp beg) (integerp end) (< beg end)
               (regexp-quote (ac-ctags-trim-whitespace
                              (buffer-substring-no-properties beg end))))))
    (when (stringp str-before-dot)
      (cond
       ((string= "this" str-before-dot)
        (ac-ctags-java-current-type-name))
       ((string-match "^[A-Z][A-Za-z_0-9]+$" str-before-dot)
        ;; probably a classname
        str-before-dot)
       ((string-match "^[_a-z][A-Za-z0-9]+$" str-before-dot)
        ;; str-before-dot seems a valid variable name
        (ac-ctags-java-extract-class-name
         (ac-ctags-java-extract-variable-line str-before-dot) str-before-dot))
       ((string-match "^[a-z.]+\\.\\([A-Za-z0-9_]+\\)$" str-before-dot)
        ;; case for "package.name.Classname."
        (match-string-no-properties 1 str-before-dot))
       (t
        (let ((identifier (ac-ctags-java-parse-before-dot-part str-before-dot)))
          (when (stringp identifier)
            (cond
             ((string-match (concat identifier "(")
                            str-before-dot)
              ;; this is a method name
              (ac-ctags-java-get-method-return-type identifier))
             ((string-match "^[A-Z][A-Za-z0-9_]+$" identifier)
              ;; case for "ClassName."
              identifier)
             (t
              ;; this is a variable name
              (ac-ctags-java-extract-class-name
               (ac-ctags-java-extract-variable-line identifier) identifier))))))))))

(defun ac-ctags-java-get-method-return-type (method-name)
  (loop with case-fold-search = nil
        for node in (ac-ctags-get-nodes-by-lang-and-name "Java" method-name)
        for name = (ac-ctags-node-name node)
        for kind = (ac-ctags-node-kind node)
        for returntype = (ac-ctags-node-returntype node)
        when (and (string= method-name name)
                  (string= kind "method"))
        do (return returntype)))

(defun ac-ctags-java-current-type-name ()
  "Return a classname where the current position is in, or nil."
  (save-excursion
    (loop with case-fold-search = nil
          with ret = nil
          while (re-search-backward (concat
                                     "\\(class\\|interface\\|enum\\)[ \t]+"
                                     "[A-Z][A-Za-z0-9_]+"
                                     )
                                    nil
                                    (point-min))
          do (setq ret (ac-ctags-java-has-type-name
                        (buffer-substring-no-properties (line-beginning-position)
                                                        (line-end-position))))
          when ret
          return ret)))

(defun ac-ctags-java-has-type-name (line)
  (when (string-match (concat
                       "\\(class\\|interface\\|enum\\)[ \t]"
                       "\\([A-Z][A-Za-z0-9_]+\\)")
                      line)
    (match-string-no-properties 2 line)))

(defun ac-ctags-java-extract-class-name (line varname)
  "Extract a classname of VARNAME from LINE and return it if found, or nil."
  (let ((case-fold-search nil))
    (when (and (stringp line)
               (stringp varname)
               (string-match (concat "\\([A-Z][A-Za-z0-9_]+\\)\\(<[^ ]+>\\)?[ \t]+"
                                     varname)
                             line))
      (match-string-no-properties 1 line))))

(defun ac-ctags-java-extract-variable-line (varname)
  "Return string which we has inferred has a typename of VARNAME."
  (or (ac-ctags-java-extract-variable-line-1 varname
                                             (point-min)
                                             (point))
      (ac-ctags-java-extract-variable-line-1 varname
                                             (1+ (point))
                                             (point-max))))

(defun ac-ctags-java-extract-variable-line-1 (varname beg end)
  (save-excursion
    (loop with case-fold-search = nil
          while (re-search-forward (concat "[ \t]" varname "[; =)]")
                                   end
                                   t)
          initially do (goto-char beg)
          when (ac-ctags-java-line-has-typeinfo-p
                varname
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position)))
          return (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position)))))

(defun ac-ctags-java-line-has-typeinfo-p (varname line)
  "Return t if this LINE contains type name, or nil."
  (let ((type-regexp (concat
                      "\\([[:alpha:]][[:alnum:]]+[.]?\\)+"
                      "\\(<[^=]*>\\)*"
                      "\\(\\[\\]\\)*"
                      "[[:space:]]+"
                      ))
        (exclude-regexp "return"))
    (and (string-match-p (concat type-regexp
                                 varname
                                 "[,=;)[:space:]]"
                                 )
                         line)
         (not (string-match-p exclude-regexp line))
         (not (string-match-p "^[[:space:]]*//" line)))))

(defun ac-ctags-java-parse-before-dot-part (string)
  "Return outermost identifier name in STRING.
STRING should be the string before dot, exclusive.

Example: if STRING is method1(), then return method1.
If STRING is (varname), then return varname.
If STRING is method1(method2()), return method1.
If STRING is method1(method2(), return method2."
  (loop named this-func
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
        else
        do (push ch identifier)
        finally (return-from this-func
                  (progn
                    (when identifier
                      (push (ac-ctags-trim-whitespace (reduce #'concat identifier))
                            stack))
                    (car (ac-ctags-java-remove-keyword stack))))))

(defun ac-ctags-java-remove-keyword (lst)
  (remove-if (lambda (str)
               (string-match (concat "return\\|"
                                     "throws\\|"
                                     "new")
                             str))
             lst))

(defun ac-ctags-java-enum-candidates ()
  "Candidates function to complete enum."
  ;;(message "DEBUG: ac-ctags-java-enum-candidates, ac-prefx=%s" ac-prefix)
  (let ((case-fold-search nil)
        (line (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (when (string-match "[ \t(.]\\([A-Z][A-Za-z0-9_]+\\)\\.[A-Za-z0-9_]*" line)
      (ac-ctags-java-enum-candidates-1 (match-string-no-properties 1 line)
                                       ac-prefix))))

(defun ac-ctags-java-enum-candidates-1 (enum-typename prefix)
  (cond
   ((null enum-typename)
    nil)
   ((or (null prefix) (zerop (length prefix)))
    (ac-ctags-java-collect-enums enum-typename ""))
   (t
    (ac-ctags-java-collect-enums enum-typename preifx))))

(defun ac-ctags-java-collect-enums (enum-typename prefix)
  (loop with ret = nil
        with case-fold-search = nil
        for node in (ac-ctags-get-nodes-by-lang-and-name "Java" prefix)
        for kind = (ac-ctags-node-kind node)
        for enum = (ac-ctags-node-enum node)
        when (and (stringp enum)
                  (stringp kind)
                  (or (string= enum enum-typename)
                      (string-match (concat "\\." enum-typename "$") enum))
                  (string= "enum constant" kind))
        do (push (ac-ctags-node-name node) ret)
        finally (return (sort ret #'string<))))

;; prefix should be like "name." or "nam"
(defun ac-ctags-java-collect-packages (prefix)
  "Return a list of package names which begin with PREFIX."
  (loop with case-fold-search = nil
        for node in (ac-ctags-get-nodes-by-lang-and-name "Java" prefix)
        for kind = (ac-ctags-node-kind node)
        for name = (ac-ctags-node-name node)
        when (and (stringp kind)
                  (stringp name)
                  (string= kind "package"))
        collect name into names
        finally (return (sort (remove-if
                               (lambda (package) (string= prefix package))
                               (remove-duplicates names :test #'string=))
                              #'string<))))

(defun ac-ctags-java-collect-classes-in-package (package)
  "Return a list of class, interface, and enum names in package PACKAGE."
  (loop with case-fold-search = nil
        for node in (ac-ctags-get-nodes-by-lang-and-name "Java" "")
        for kind = (ac-ctags-node-kind node)
        for name = (ac-ctags-node-name node)
        for file = (ac-ctags-node-file node)
        when (and (stringp kind)
                  (stringp name)
                  (stringp file)
                  (or (string= kind "class")
                      (string= kind "interface")
                      (string= kind "enum"))
                  (string-match (concat
                                 (replace-regexp-in-string "\\." "/" package)
                                 "/"
                                 name)
                                file))
        collect name into names
        finally (return (sort names #'string<))))

(defun ac-ctags-java-package-candidates-1 (prefix)
  (let ((case-fold-search nil))
    (cond
     ((string-match "\\.$" prefix)
      ;; collect packages under prefix and classes in a package.
      (append (ac-ctags-java-collect-packages (substring-no-properties
                                               prefix
                                               0
                                               (1- (length prefix))))
              ;; we have to prepend package name to class name
              (mapcar (lambda (class)
                        (concat prefix class))
                      (ac-ctags-java-collect-classes-in-package (substring-no-properties
                                                                 prefix
                                                                 0
                                                                 (1- (length prefix)))))))
     ((string-match "^[a-z.]+$" prefix)
      ;; this is a package name
      (ac-ctags-java-collect-packages prefix))
     ((string-match "\\([a-z.]+\\)\\.\\([A-Z][A-Za-z_0-9]+\\)$" prefix)
      (let ((package-name (match-string-no-properties 1 prefix)))
        ;; this is a package name + class name
        (mapcar (lambda (class)
                  (concat package-name "." class))
                (all-completions (match-string-no-properties 2 prefix)
                                 (ac-ctags-java-collect-classes-in-package
                                  (match-string-no-properties 1 prefix)))
                )))
     (t
      nil))))

(defun ac-ctags-java-package-candidates ()
  "Candidate function for java package names."
  (let ((case-fold-search nil))
    (when (and (stringp ac-prefix)
               (string-match "[a-z.]+\\([A-Z][a-zA-Z_0-9]+\\)*" ac-prefix))
      (ac-ctags-java-package-candidates-1 ac-prefix))))

(defun ac-ctags-java-package-prefix ()
  (save-excursion
    (when (re-search-backward "\\([[:space:](]\\|^\\)"
                              (line-beginning-position)
                              t)
      (match-end 1))))

(defun ac-ctags-java-constructor-prefix ()
  (let ((case-fold-search nil))
    (cond
     ((save-excursion
        (re-search-backward "new[ \t]+\\([A-Z][A-Za-z0-9_]+\\)" (line-beginning-position) t))
      (match-beginning 1))
     ((save-excursion
        (re-search-backward "new[ \t][a-z.]+\\([A-Z][A-Za-z0-9]+\\)" (line-beginning-position) t))
      (match-beginning 1))
     (t
      nil))))

(defun ac-ctags-java-constructor-candidates ()
  ;;(message "DEBUG: java-ctor-candidates, ac-prefix=%s" ac-prefix)
  (ac-ctags-java-collect-constructors ac-prefix))

(defun ac-ctags-java-collect-constructors (prefix)
  (let ((case-fold-search nil))
    (when (string-match-p "^[A-Z]" prefix)
      (loop for node in (ac-ctags-get-nodes-by-lang-and-name "Java" prefix)
            for name = (ac-ctags-node-name node)
            for kind = (ac-ctags-node-kind node)
            when (and (stringp name)
                      (stringp kind)
                      (string= kind "method"))
            collect (ac-ctags-java-make-method-candidate node) into ctors
            finally (return (sort ctors #'string<))))))

;; Definitions of each ac-source for java
(ac-define-source ctags-java-method
  '((candidates . ac-ctags-java-method-candidates)
    (cache)
    (candidate-face . ac-ctags-candidate-face)
    (selection-face . ac-ctags-selection-face)
    (requires . 0)
    (prefix . "\\.\\(.*\\)")
    (action . ac-ctags-java-method-action)))

(ac-define-source ctags-java-constructor
  '((candidates . ac-ctags-java-constructor-candidates)
    (cache)
    (candidate-face . ac-ctags-candidate-face)
    (selection-face . ac-ctags-selection-face)
    (requires . 2)
    (prefix . ac-ctags-java-constructor-prefix)
    (action . ac-ctags-java-method-action)))

(ac-define-source ctags-java-package
  '((candidates . ac-ctags-java-package-candidates)
    (cache)
    (candidate-face . ac-ctags-candidate-face)
    (selection-face . ac-ctags-selection-face)
    (requires . 2)
    (prefix . ac-ctags-java-package-prefix)))

(ac-define-source ctags-java-enum
  '((candidates . ac-ctags-java-enum-candidates)
    (cache)
    (candidate-face . ac-ctags-candidate-face)
    (selection-face . ac-ctags-selection-face)
    (requires . 0)
    (prefix . "\\.\\(.*\\)")))

(ac-define-source ctags-java-field
  '((candidates . ac-ctags-java-field-candidates)
    (cache)
    (candidate-face . ac-ctags-candidate-face)
    (selection-face . ac-ctags-selection-face)
    (requires . 0)
    (prefix . "\\.\\(.*\\)")))

(defun ac-ctags-java-toggle ()
  (interactive)
  (let ((sources '(ac-source-ctags-java-method
                   ac-source-ctags-java-enum
                   ac-source-ctags-java-field
                   ac-source-ctags-java-package
                   ac-source-ctags-java-constructor)))
    (setq ac-sources
          (if (intersection sources ac-sources)
              (set-difference ac-sources sources)
            (append sources ac-sources)))))

(provide 'auto-complete-ctags-java)
;;; auto-complete-ctags-java.el ends here
