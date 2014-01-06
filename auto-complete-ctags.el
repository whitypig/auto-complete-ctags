;;; auto-complete-ctags.el ---

;; Copyright (C) 2011  whitypig

;; Author: whitypig <whitypig@gmail.com>
;; Keywords: auto-complete-mode, ctags

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

(require 'auto-complete)
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup auto-complete-ctags nil
  "A source for auto-complete-mode usign Exuberant ctags."
  :prefix "ac-ctags-"
  :group 'convenience)

(defcustom ac-ctags-candidate-limit 50
  "The upper limit number of candidates to be shown."
  :type 'number
  :group 'auto-complete-ctags)

(defcustom ac-ctags-mode-to-string-table
  '((c-mode ("C"))
    (c++-mode ("C++" "C"))
    (java-mode ("Java"))
    (jde-mode ("Java"))
    (malabar-mode ("Java"))
    (php-mode ("PHP")))
  "A table for mapping major-mode to its representing string."
  :type 'list
  :group 'auto-complete-ctags)

(defcustom ac-ctags-vector-default-size 1023
  "The default size of vector used as completion table"
  :type 'number
  :group 'auto-complete-ctags)

(defcustom ac-ctags-candidate-default-width 40
  "Default width of each candidate that has text property."
  :type 'number
  :group 'auto-complete-ctags)

(defcustom ac-ctags-cache-dir "~/.ac-ctags"
  "Directory name where cache files are stored."
  :type 'directory
  :group 'auto-complete-ctags)

(defface ac-ctags-candidate-face
  '((t (:background "slate gray" :foreground "white")))
  "Face for ctags candidate")

(defface ac-ctags-selection-face
  '((t (:background "PaleGreen4" :foreground "white")))
  "Face for the ctags selected candidate.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ac-ctags-current-tags-list nil
  "Current list of tags.")

(defvar ac-ctags-tags-list-set nil
  "The set of lists of tags files.")

(defvar ac-ctags-tags-db nil
  "An association list with keys being languages and values being
the information extracted from tags file created by ctags
program. The following is an example:
`((\"C++\" (name command kind class signature)...)
  (\"C\" (name command kind signature)...)
  (\"Java\" (name command kind class signature)...)
  (\"Others\" (name command kind signature)...)'")

(defvar ac-ctags-completion-table nil
  "An association list with keys being a language and values being a
vector containing tag names in tags files. The following is an
example.
`((\"C++\" . [name1 name2 name3...])
  (\"Java\" . [name1 name2 name3...])
  (\"Others\" . [name1 name2 name3)])'")

(defvar ac-ctags-current-completion-table nil
  "A vector used for completion for `ac-ctags-current-tags-list'.")

(defvar ac-ctags-tags-db-created-time nil
  "Creation time of `ac-ctags-tags-db'")

(defvar ac-ctags-prefix-funtion-table
  '((c++-mode . ac-ctags-c++-prefix))
  "A table of prefix functions for a specific major mode.")

(defvar ac-ctags-document-function-table
  '((c++-mode . ac-ctags-c++-document)
    (c-mode . ac-ctags-c-document)
    (java-mode . ac-ctags-java-document)
    (jde-mode . ac-ctags-java-document)
    (malabar-mode . ac-ctags-java-document))
  "A table of document functions")

(defvar ac-ctags-current-major-mode nil
  "Current major mode")

(defvar ac-ctags-ac-sources-alist
  '((java-mode . (ac-source-ctags-java-method
                  ac-source-ctags-java-enum
                  ac-source-ctags-java-field
                  ac-source-ctags-java-package))))

(defvar ac-ctags-split-list-size 500)

(defconst ac-ctags-no-document-message "No document available.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ac-ctags-visit-tags-file (file &optional new)
  "Visit tags file."
  (interactive (list (expand-file-name
                      (read-file-name "Visit tags file (default tags): "
                                      nil
                                      "tags"
                                      t))))
  (or (stringp file) (signal 'wrong-type-argument (list 'stringp file)))
  (let ((tagsfile file))
    (unless (ac-ctags-is-valid-tags-file-p tagsfile)
      (error "Invalid tags: %s is not a valid tags file" tagsfile))
    (ac-ctags-build tagsfile new)
    (message "Current tags list: %s" ac-ctags-current-tags-list)))

(defun ac-ctags-update-tags-list (tagsfile new)
  "Update `ac-ctags-current-tags-list' and`ac-ctags-tags-list-set' if
need be."
  (cond
   ((null ac-ctags-current-tags-list)
    (ac-ctags-insert-tags-into-current-list tagsfile))
   ;; Ask user whether the tags will be inserted into the current
   ;; list or a new one, and do insert.
   ((or (eq new 'new)
        (and (not (eq new 'current))
             (ac-ctags-create-new-list-p tagsfile)))
    (ac-ctags-insert-tags-into-new-list tagsfile))
   (t
    (ac-ctags-insert-tags-into-current-list tagsfile))))

(defun ac-ctags-build (tagsfile new)
  "Update current tags list and build db."
  (let (db
        tbl
        (vec (make-vector ac-ctags-vector-default-size 0))
        (old-lst ac-ctags-current-tags-list)
        new-lst)
    (setq new-lst (ac-ctags-update-tags-list tagsfile new))
    (ac-ctags-build-1 old-lst new-lst)))

(defun ac-ctags-build-1 (old-lst new-lst)
  "If tags list has changed, rebuild db."
  (let (db
        tbl
        (vec (make-vector ac-ctags-vector-default-size 0)))
    (unless (or (null new-lst) (equal old-lst new-lst))
      ;; If tags list has changed, we update the information
      (setq db (ac-ctags-build-tagsdb new-lst db))
      (setq tbl (ac-ctags-build-completion-table db))
      (setq vec (ac-ctags-build-current-completion-table vec tbl))
      ;; Update the state
      (setq ac-ctags-current-tags-list new-lst
            ac-ctags-tags-db db
            ac-ctags-completion-table tbl
            ac-ctags-current-completion-table vec
            ac-ctags-tags-db-created-time (nbutlast (current-time))))))

(defun ac-ctags-create-new-list-p (tagsfile)
  "Ask user whether to create the new tags file list or use the
current one. TAGSFILE is guaranteed to be a valid tagfile."
  ;; Check if TAGSFILE is already in the current list.
  (if (and ac-ctags-current-tags-list
           (member tagsfile ac-ctags-current-tags-list))
      (y-or-n-p "The tags file is already in the current tags list.\nCreate a new list? ")
    ;; If not in the list, ask the user what to do.
    (y-or-n-p "Create new tags list? ")))

(defun ac-ctags-insert-tags-into-new-list (tagsfile)
  "Insert TAGSFILE into a new tags list and return the current
list."
  (setq ac-ctags-current-tags-list (list tagsfile))
  (unless (member ac-ctags-current-tags-list
                  ac-ctags-tags-list-set)
    (push ac-ctags-current-tags-list ac-ctags-tags-list-set))
  ac-ctags-current-tags-list)

(defun ac-ctags-insert-tags-into-current-list (tagsfile)
  "Insert TAGSFILE into the current tags list and return the
current list."
  (unless (member tagsfile ac-ctags-current-tags-list)
    (setq ac-ctags-tags-list-set
          (delete ac-ctags-current-tags-list ac-ctags-tags-list-set))
    (push tagsfile ac-ctags-current-tags-list)
    (push ac-ctags-current-tags-list ac-ctags-tags-list-set)
    ac-ctags-current-tags-list))

(defun ac-ctags-is-valid-tags-file-p (tags)
  "Return t if TAGS is valid tags file created by exuberant
ctags."
  (let ((fullpath (and tags (file-exists-p tags) (expand-file-name tags)))
        (needle "!_TAG_PROGRAM_NAME	Exuberant Ctags"))
    (when fullpath
      (with-temp-buffer
        ;; So far we think 500 is enough.
        (insert-file-contents-literally fullpath nil 0 500)
        (search-forward needle nil t)))))

(defun ac-ctags-build-tagsdb (tags-list tags-db)
  "Build tagsdb from each element of TAGS-LIST."
  (dolist (e tags-list tags-db)
    (if (ac-ctags-tags-file-updated-p e)
        ;; if this tags file has been updated,
        ;; we have to rebuild tagsdb for this tags file
        (setq tags-db (ac-ctags-build-tagsdb-from-tags e tags-db))
      ;; otherwise, read db from cache if it exists
      (setq tags-db (or (ac-ctags-read-tagsdb-from-cache e tags-db)
                        (ac-ctags-build-tagsdb-from-tags e tags-db))))))

(defun ac-ctags-tags-file-updated-p (tags-file)
  "Return t if cache file doesn't exist or the modified time of
TAGS-FILE is newer than that of cache file."
  (let ((cache-pathname (ac-ctags-get-cache-file tags-file))
        (file-created-time (nth 5 (file-attributes (expand-file-name tags-file)))))
    (or (not (file-exists-p cache-pathname))
        (time-less-p (ac-ctags-get-cache-created-time cache-pathname)
                     file-created-time))))

(defun ac-ctags-get-cache-created-time (pathname)
  "Return created time of this cache file PATHNAME."
  (assert (file-exists-p pathname) "ac-ctags-get-cache-created-time")
  (nth 5 (file-attributes pathname)))

(defun ac-ctags-read-tagsdb-from-cache (tags-file tags-db)
  "Read tagsdb from cache and return merged TAGS-DB.
Cache file, if exists, corresponds to one tags file"
  (when (ac-ctags-cache-file-exist-p tags-file)
    ;; read from cache and do merge.
    (let ((db nil)
          (lang nil)
          (obj nil)
          (lst nil))
      (with-current-buffer (find-file-noselect
                            (ac-ctags-get-cache-file tags-file) t)
        (goto-char (point-min))
        (message "Reading db for %s from cache..." (file-name-nondirectory tags-file))
        ;; suppress eof signal error from #'read
        (while (and (not (eq :eof (setq obj (ignore-errors (read (current-buffer))))))
                    (not (null obj)))
          (cond
           ((stringp obj)
            (setq lang obj)
            (unless (assoc lang db)
              (push (list obj) db)))
           ((listp obj)
            (assert lang)
            (mapcar (lambda (e)
                      (push e (cdr (assoc lang db))))
                    obj))
           (t
            (error "ac-ctags-read-tagsdb-from-cache"))))
        (kill-buffer))
      (message "Reading db for %s from cache...done" (file-name-nondirectory tags-file))
      (setq tags-db (ac-ctags-merge-db db tags-db)))))

(defun ac-ctags-cache-file-exist-p (tags-file)
  (file-exists-p (concat (directory-file-name (expand-file-name ac-ctags-cache-dir))
                         "/"
                         (sha1 (expand-file-name tags-file))
                         ".acctags")))

(defun ac-ctags-get-cache-file (tags-file)
  "Return a cache filename for TAGS-FILE."
  (let* ((cache-name (sha1 (expand-file-name tags-file)))
         (cache-pathname (concat (directory-file-name (expand-file-name ac-ctags-cache-dir))
                                 "/"
                                 cache-name)))
    (concat cache-pathname ".acctags")))

(defun ac-ctags-build-tagsdb-from-tags (tags tags-db)
  "Build tag information db from TAGS and return the db.
Each element of DB is a list like (name cmd signature) where NAME
  is tag name, CMD is info constructed from EX command, and
  SIGNATURE is as is. If NAME entry has no signature, then
  SIGNATURE is nil.
TAGS is expected to be an absolute path name."
  (assert (ac-ctags-is-valid-tags-file-p tags))
  (let ((db nil))
    (with-temp-buffer
      (insert-file-contents-literally tags)
      (goto-char (point-min))
      (let ((reporter (make-progress-reporter
                       (format "Building tags db for %s..."
                               (file-name-nondirectory tags))
                       (point-min) (point-max))))
        ;; todo: How can we get the return type? `signature' in tags file
        ;; does not contain the return type.
        (while (re-search-forward
                "^\\([^!\t]+\\)\t\\([^\t]+\\)\t\\(.*\\);\"\t.*$"
                nil t)
          (let (line name file cmd kind (lang "Others") signature
                     class interface enum returntype namespace)
            (setq line (match-string-no-properties 0)
                  name (match-string-no-properties 1)
                  file (match-string-no-properties 2)
                  cmd (ac-ctags-trim-whitespace
                       (ac-ctags-strip-cmd (match-string-no-properties 3))))
            ;; If this line contains a language information, we get it.
            (when (string-match "language:\\([^\t\n]+\\)" line)
              (setq lang (match-string-no-properties 1 line)))
            ;; If this line contains a signature, we get it.
            (when (string-match "signature:\\([^\t\n]+\\)" line)
              (setq signature (match-string-no-properties 1 line)))
            (when (string-match "kind:\\([^\t\n]+\\)" line)
              (setq kind (match-string-no-properties 1 line)))
            (when (string-match "class:\\([^\t\n]+\\)" line)
              (setq class (match-string-no-properties 1 line)))
            (when (string-match "interface:\\([^\t\n]+\\)" line)
              (setq interface (match-string-no-properties 1 line)))
            (when (string-match "enum:\\([^\t\n]+\\)" line)
              (setq enum (match-string-no-properties 1 line)))
            (when (string-match "returntype:\\([^\t\n]+\\)" line)
              (setq returntype (match-string-no-properties 1 line)))
            (when (string-match "namespace:[:]?\\([^\t\n]+\\)" line)
              (setq namespace (match-string-no-properties 1 line)))
            (if (assoc lang db)
                (push `(,name ,file ,cmd ,kind ,class ,interface ,signature ,enum ,returntype ,namespace)
                      (cdr (assoc lang db)))
              (push `(,lang (,name ,file ,cmd ,kind ,class ,interface ,signature ,enum ,returntype ,namespace))
                    db)))
          (progress-reporter-update reporter (point)))
        (progress-reporter-done reporter)))
    (ac-ctags-write-db-to-cache tags db)
    (setq tags-db (ac-ctags-merge-db db tags-db))))

(defun ac-ctags-write-db-to-cache (tags-file db)
  "Write DB into cache."
  (with-temp-file (ac-ctags-get-cache-file tags-file)
    (let ((print-circle t))
      (message "Writing db for %s to cache..." (file-name-nondirectory tags-file))
      (loop for lang-alist in db
            for lang = (car lang-alist)
            for lang-db = (cdr lang-alist)
            do (print lang (current-buffer))
            do (loop for l in (ac-ctags-split-list lang-db ac-ctags-split-list-size)
                     do (print l (current-buffer))))
      (message "Writing db for %s to cache...done" (file-name-nondirectory tags-file)))))

(defun ac-ctags-merge-db (db tags-db)
  "Merge DB into TAGS-DB."
  (loop for lang-db in db
        for lang = (car lang-db)
        do (if (assoc lang tags-db)
               (mapcar (lambda (entry)
                         (push entry (cdr (assoc lang tags-db))))
                       (cdr lang-db))
             (push lang-db tags-db))
        finally (return tags-db)))

(defun ac-ctags-tagsdb-needs-update-p (db-created-time)
  "Return t if DB-CRETED-TIME is older than any one of file
modification times of a tags file in `ac-ctags-current-tags-list'."
  (some (lambda (time)
          (time-less-p db-created-time time))
        (mapcar (lambda (tags)
                  (nth 5 (file-attributes tags)))
                ac-ctags-current-tags-list)))

(defun ac-ctags-update-tagsdb (db-created-time &optional force)
  "Update tagsdb, completion table, and so on if need be."
  (when (or force
            (null db-created-time)
            (ac-ctags-tagsdb-needs-update-p db-created-time))
    (let (db
          tbl
          (vec (make-vector ac-ctags-vector-default-size 0)))
      ;; If tags list has changed, we update the information
      (setq db (ac-ctags-build-tagsdb ac-ctags-current-tags-list db))
      (setq tbl (ac-ctags-build-completion-table db))
      (setq vec (ac-ctags-build-current-completion-table vec tbl))
      ;; Update the state
      (setq ac-ctags-tags-db db
            ac-ctags-completion-table tbl
            ac-ctags-current-completion-table vec
            ac-ctags-tags-db-created-time (nbutlast (current-time))))))

(defun ac-ctags-make-signature (tag-signature)
  "Return string of signature created using TAG-SIGNATURE."
  (cond
   ((string= tag-signature "()")
    tag-signature)
   ((string= tag-signature "(void)")
    "()")
   (t
    (let ((str (substring-no-properties tag-signature
                                        1
                                        (1- (length tag-signature)))))
      (concat "("
              (reduce (lambda (x y)
                        (concat x ", " y))
                      (mapcar (lambda (l)
                                (if (> (length l) 2)
                                    (nth 1 l)
                                  (car l)))
                              (mapcar #'split-string
                                      (split-string str "[,]"))))
              ")")))))

;; Node accessor functions.
;; Node represents one line in tags file.
(defun ac-ctags-node-name (node)
  (car node))

(defun ac-ctags-node-file (node)
  (nth 1 node))

(defun ac-ctags-node-command (node)
  (nth 2 node))

(defun ac-ctags-node-kind (node)
  (nth 3 node))

(defun ac-ctags-node-class (node)
  (nth 4 node))

(defun ac-ctags-node-interface (node)
  (nth 5 node))

(defun ac-ctags-node-signature (node)
  (nth 6 node))

(defun ac-ctags-node-enum (node)
  (nth 7 node))

(defun ac-ctags-node-returntype (node)
  (nth 8 node))

(defun ac-ctags-node-namespace (node)
  (nth 9 node))

;; ("C++" (name command signature)...)
(defun ac-ctags-build-completion-table (tags-db)
  "TAGS-DB must be created by ac-ctags-build-tagdb beforehand."
  (let ((tbl nil))
    (dolist (db tags-db)
      ;; Create completion table for each language.
      (let ((lang (car db)) (names (mapcar #'ac-ctags-node-name (cdr db))))
        (if (assoc lang tbl)
            ;; intern each name into the vector
            (mapc (lambda (name) (intern name (cdr (assoc lang tbl))))
                  names)
          (let ((vec (make-vector ac-ctags-vector-default-size 0)))
            (mapc (lambda (name) (intern name vec)) names)
            (push (cons lang vec) tbl)))))
    tbl))

(defun ac-ctags-build-current-completion-table (vec table)
  "Build completion vector"
  (let ((langs (ac-ctags-get-mode-string major-mode)))
    (dolist (l langs)
      (when (cdr (assoc l table))
        (mapatoms (lambda (sym)
                    (intern (symbol-name sym) vec))
                  (cdr (assoc l table)))))
    vec))

(defun ac-ctags-update-current-completion-table (mode)
  "Switch completion table. MODE represents the current major mode."
  (when (ac-ctags-major-mode-has-changed-p mode)
    (let ((vec (make-vector ac-ctags-vector-default-size 0)))
      (setq vec (ac-ctags-build-current-completion-table
                 vec
                 ac-ctags-completion-table))
      (setq ac-ctags-current-completion-table vec
            ac-ctags-current-major-mode mode)
      )))

(defun ac-ctags-major-mode-has-changed-p (mode)
  (or (null ac-ctags-current-major-mode)
      (not (eq mode ac-ctags-current-major-mode))))

(defun ac-ctags-update-ac-sources (from-mode to-mode)
  "Add and remove sources to and from `ac-sources' depending on
FROM-MODE and TO-MODE."
  (unless (eq from-mode to-mode)
    (let ((sources-to-remove (ac-ctags-get-ac-sources-by-mode from-mode))
          (sources-to-add (ac-ctags-get-ac-sources-by-mode to-mode)))
      (setq ac-sources
            (nunion (nset-difference ac-sources sources-to-remove)
                    sources-to-add)))))

(defun ac-ctags-trim-whitespace (str)
  "Trim prepending and trailing whitespaces and return the result
  string."
  (replace-regexp-in-string "[ \t]+$" ""
                            (replace-regexp-in-string "^[ \t]+" "" str)))

(defun ac-ctags-strip-cmd (str)
  (let ((ret (replace-regexp-in-string "^/^" ""
                                       (replace-regexp-in-string "\\$/$" "" str))))
    (replace-regexp-in-string ";$" "" ret)))

(defun ac-ctags-get-signature (name db lang)
  "Return a list of signatures corresponding to NAME."
  (loop for e in (cdr (assoc lang db))
        ;; for each `(name cmd kind signature)'
        ;; linear searching is not what I want to use...
        when (and (string= name (ac-ctags-node-name e))
                  (ac-ctags-node-signature e))
        collect (ac-ctags-construct-signature e)))

(defun ac-ctags-get-signature-by-mode (name db mode)
  "Return a list containing signatures corresponding `name'."
  (let ((langs (ac-ctags-get-mode-string mode))
        (sigs nil))
    (when langs
      (dolist (lang langs)
        (let ((siglst (ac-ctags-get-signature name db lang)))
          (when siglst
            (setq sigs (append siglst sigs))))))
    (sort sigs #'string<)))

(defun ac-ctags-construct-signature (node)
  "Construct a full signature if possible."
  ;; TODO
  ;; deal with "throws" in cpp
  ;; deal with "public" and so on in java
  (concat (ac-ctags-node-returntype node)
          (and (ac-ctags-node-returntype node) " ")
          (ac-ctags-node-name node)
          (ac-ctags-node-signature node)))

(defun ac-ctags-has-signature-p (kind)
  (or (string= kind "function")
      (string= kind "prototype")
      (string= kind "method")))

(defun ac-ctags-strip-class-name (name)
  (cond
   ;; For c++
   ((string-match ".*::\\([^:]+\\)" name)
    (match-string-no-properties 1 name))
   ;; For java
   ((string-match ".*\\.\\([^.]+\\)" name)
    (match-string-no-properties 1 name))
   (t name)))

(defun ac-ctags-reset ()
  "Reset tags list, set, and other data."
  (interactive)
  (setq ac-ctags-current-tags-list nil
        ac-ctags-tags-list-set nil
        ac-ctags-tags-db nil
        ac-ctags-completion-table nil
        ac-ctags-current-completion-table nil))

(defun ac-ctags-get-mode-string (mode)
  (or (cadr (assoc mode ac-ctags-mode-to-string-table))
      '("Others")))

(defun ac-ctags-get-ac-sources-by-mode (mode)
  (cdr (assoc mode ac-ctags-ac-sources-alist)))

(defun ac-ctags-split-list (lst n)
  "Split LST by N elements."
  (loop for l = lst then (nthcdr n l)
        for len = (length l)
        while l
        collect (butlast l (- len n))))

;;;;;;;;;;;;;;;;;;;; ac-ctags-select-tags-list-mode ;;;;;;;;;;;;;;;;;;;;
(require 'button)

(defvar ac-ctags-select-tags-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-buffer-map)
    (define-key map "t" 'push-button)
    (define-key map "j" 'next-line)
    (define-key map "\C-i" 'next-line)
    (define-key map "k" 'previous-line)
    (define-key map "q" 'ac-ctags-select-tags-list-quit)
    map))

(define-button-type 'ac-ctags-select-tags-list-button-type
  'action 'ac-ctags-select-tags-list-select
  'help-echo "RET, t, or mouse-2 to select tags list")

(define-derived-mode ac-ctags-select-tags-list-mode fundamental-mode "Select Tags List"
  "Major mode for selecting a current tags list.

\\{ac-ctags-select-tags-list-mode-map}"
  (setq buffer-read-only t))

(defun ac-ctags-select-tags-list ()
  "Swith to another list of tags."
  (interactive)
  (let ((beg nil) (b nil))
    (setq ac-ctags-window-conf (current-window-configuration))
    (pop-to-buffer "*auto-complete-ctags*")
    (erase-buffer)
    (goto-char (point-min))
    (insert "Type t or Enter on the list you want to use.")
    (newline)
    (newline)
    (setq beg (point))
    (setq b (point))
    ;; First, print the current list on the top of this buffer.
    (princ (mapcar #'abbreviate-file-name ac-ctags-current-tags-list)
           (current-buffer))
    (make-text-button b (point) 'type 'ac-ctags-select-tags-list-button-type
                      'ac-ctags-tags-list ac-ctags-current-tags-list)
    (newline)
    ;; Then, print the rest.
    (when (and ac-ctags-tags-list-set
               (car ac-ctags-tags-list-set)
               (not (null (car (remove ac-ctags-current-tags-list ac-ctags-tags-list-set)))))
      (loop for e in (remove ac-ctags-current-tags-list ac-ctags-tags-list-set)
            do (progn
                 (setq b (point))
                 (princ (mapcar #'abbreviate-file-name e) (current-buffer))
                 (make-text-button b (point) 'type 'ac-ctags-select-tags-list-button-type
                                   'ac-ctags-tags-list e)
                 (newline))))
    (goto-char beg)
    (ac-ctags-select-tags-list-mode)))

(defun ac-ctags-select-tags-list-select (button)
  "Select the tags list on this line."
  (interactive (list (or (button-at (line-beginning-position))
                         (error "No tags list on the current line"))))
  (let ((new-lst (button-get button 'ac-ctags-tags-list))
        (old-lst ac-ctags-current-tags-list))
    (ac-ctags-select-tags-list-quit)
    ;; If the newly selected tags list is not the same as the current
    ;; one, we switch the current list to the new one.
    (when (and new-lst
               (not (equal ac-ctags-current-tags-list
                           new-lst))
               (ac-ctags-switch old-lst new-lst))
      (message "Current tags list: %s" new-lst))))

(defun ac-ctags-switch (old-lst new-lst)
  (ac-ctags-build-1 old-lst new-lst))

(defun ac-ctags-select-tags-list-quit ()
  (interactive)
  (quit-window t (selected-window))
  (set-window-configuration ac-ctags-window-conf))

(defun ac-ctags-update ()
  "Use this command to update tags db, completion tables and so
on when you have updated tags file."
  (interactive)
  (ac-ctags-build-1 nil ac-ctags-current-tags-list))

;;;;;;;;;;;;;;;;;;;; Candidates functions ;;;;;;;;;;;;;;;;;;;;
(defun ac-ctags-candidates ()
  ;;(message "ac-ctags-candidates, ac-prefix=%s" ac-prefix)
  (ac-ctags-update-ac-sources ac-ctags-current-major-mode major-mode)
  (ac-ctags-candidates-1 ac-prefix))

(defun ac-ctags-candidates-1 (prefix)
  (ac-ctags-update-tagsdb ac-ctags-tags-db-created-time)
  (ac-ctags-update-current-completion-table major-mode)
  (when (stringp prefix)
    (setq candidates
          (sort (ac-ctags-collect-candidates prefix)
                #'string<)
          ;; (sort (all-completions prefix ac-ctags-current-completion-table)
          ;;       #'string<)
          )
    (let ((len (length candidates)))
      (if (and candidates
               (numberp ac-ctags-candidate-limit)
               (> len ac-ctags-candidate-limit))
          (nbutlast candidates (- len ac-ctags-candidate-limit))
        candidates))))

(defun ac-ctags-collect-candidates (prefix)
  "Collect candidates which begin with PREFIX.
Also, if a candidate is of type functin or prototype and has a
signature, make a candidate with its signature as well as
yasnippet template if possible."
  (loop for node in (ac-ctags-get-lang-db
                     (car (ac-ctags-get-mode-string major-mode)))
        for name = (ac-ctags-node-name node)
        for kind = (ac-ctags-node-kind node)
        when (string-match (concat "^" prefix) name)
        collect (if (member kind '("function" "prototype"))
                    (ac-ctags-make-function-candidate node)
                  name)))

(defun ac-ctags-make-function-candidate (node)
  "Make function candidates with its signature and return type
being properly concatenated."
  (let* ((raw-signature (ac-ctags-node-signature node))
         (signature (when (stringp raw-signature)
                      (if (string-match "void" raw-signature)
                          ;; If signature is "(void)", we use "()"
                          ;; because they are calling this function,
                          ;; not declearing or defining one.
                          "()"
                        raw-signature)))
         (ret (concat (ac-ctags-node-name node) signature))
         (returntype (when (stringp (ac-ctags-node-returntype node))
                       (concat ":" (ac-ctags-node-returntype node))))
         (viewprop returntype))
    (propertize ret
                'view (concat ret
                              (ac-ctags-get-spaces-to-insert ret viewprop)
                              returntype)
                'signature (ac-ctags-node-signature node))))

(defun ac-ctags-skip-to-delim-backward ()
  (let ((bol (save-excursion (beginning-of-line) (point)))
        (cont t))
    (while (and cont (search-backward "::" bol t))
      (when (and (char-before) (string-match "[[:alpha:]]" (string (char-before))))
        ;; skip a namespace
        (skip-chars-backward "^* \t;()<>" bol)
        (setq cont nil)))))

(defun ac-ctags-double-colon-p (pos)
  "Return t if characters at position POS and POS+1 are colons."
  (let ((c1 (char-after pos))
        (c2 (char-after (1+ pos))))
    (and (characterp c1)
         (characterp c2)
         (char-equal c1 ?:)
         (char-equal c2 ?:))))

(defun ac-ctags-make-yasnippet-template-from-signature (signature)
  (if (string= signature "()")
      "()$0"
    (concat "("
            (reduce (lambda (x y)
                      (concat x ", " y))
                    (loop for e in (ac-ctags-split-signature-string signature ",")
                          for i from 1
                          collect (format "${%d:%s}" i e)))
            ")$0")))

(defun ac-ctags-split-signature-string (signature sep-regexp)
  "Split SIGNATURE string by SEP-REGEXP, which is usually \",\".
SIGNATURE must be like \"(int i, int j)\"."
  (loop with case-fold-search = nil
        with ret = nil
        with n-angle-brackets = 0
        with acc = nil
        ;; remove first and last parens
        for ch in (cdr (nbutlast (split-string signature "" t)))
        if (and (string-match sep-regexp ch)
                (zerop n-angle-brackets))
        do (progn (push (reduce #'concat (nreverse acc)) ret)
                  (setq acc nil
                        n-angle-bracket 0))
        else
        do (cond
            ((string= ch "<")
             (incf n-angle-brackets)
             (push ch acc))
            ((string= ch ">")
             (decf n-angle-brackets)
             (push ch acc))
            (t
             (push ch acc)))
        finally (return
                 (mapcar #'ac-ctags-trim-whitespace
                         (remove ""
                                 (nreverse (push (reduce #'concat (nreverse acc)) ret)))))))

(defun ac-ctags-get-spaces-to-insert (string prop)
  "Return spaces to insert between candidate name and view property."
  (if (< ac-ctags-candidate-default-width
         (+ (length string) (length prop)))
      ;; 4 is just my choice
      (make-string 4 ? )
    (make-string (- ac-ctags-candidate-default-width
                    (length string)
                    (length prop))
               ? )))

(defun ac-ctags-get-lang-db (lang)
  (cdr (assoc lang ac-ctags-tags-db)))

;;;;;;;;;;;;;;;;;;;; Prefix functions ;;;;;;;;;;;;;;;;;;;;
(defun ac-ctags-get-prefix-function (mode table)
  (let ((f (assoc mode table)))
    (if f (cdr f)
      #'ac-prefix-symbol)))

(defun ac-ctags-prefix ()
  (or (funcall (ac-ctags-get-prefix-function major-mode ac-ctags-prefix-funtion-table))
      (ac-prefix-symbol)))

(defun ac-ctags-c++-prefix ()
  (let ((c (char-before))
        (bol (save-excursion (beginning-of-line) (point))))
    (cond
     ((and (characterp c) (char-equal c ?:))
      ;; Has just entered `::' ?
      (when (and (char-before (1- (point)))
                 (char-equal (char-before (1- (point))) ?:))
        (save-excursion
          (ac-ctags-skip-to-delim-backward)
          (if (and (= (point) bol)
                   (ac-ctags-double-colon-p (point)))
              (+ 2 (point))
            (point)))))
     ;; There is `::' on the currently-editing line,
     ;; and has just entered a character other than `:'.
     ((save-excursion
        (re-search-backward "::"
                            (save-excursion
                              (ac-ctags-skip-to-delim-backward)
                              (point))
                            t))
      (save-excursion
        (ac-ctags-skip-to-delim-backward)
        (if (ac-ctags-double-colon-p (point))
            (+ 2 (point))
          (point))))
     (t nil))))

;;;;;;;;;;;;;;;;;;;; Document functions ;;;;;;;;;;;;;;;;;;;;
(defun ac-ctags-document (item)
  (let ((func (ac-ctags-get-document-function major-mode ac-ctags-document-function-table)))
    (when func
      (funcall func item))))

(defun ac-ctags-get-document-function (mode table)
  (cdr (assoc mode table)))

(defun ac-ctags-c++-document (item)
  "Document function for c++-mode."
  (let ((lst (ac-ctags-get-signature-by-mode (substring-no-properties item)
                                             ac-ctags-tags-db
                                             'c++-mode)))
    (cond
     ((= (length lst) 1) (car lst))
     ((> (length lst) 1)
      (reduce (lambda (x y) (concat x "\n" y)) lst))
     (t ac-ctags-no-document-message))))

(defun ac-ctags-c-document (item)
  "Document function for c-mode."
  (let ((lst (ac-ctags-get-signature-by-mode (substring-no-properties item)
                                             ac-ctags-tags-db
                                             'c-mode)))
    (cond
     ((= (length lst) 1) (car lst))
     ((> (length lst) 1)
      (reduce (lambda (x y) (concat x "\n" y)) lst))
     (t ac-ctags-no-document-message))))

(defun ac-ctags-java-document (item)
  "Document function for java-related mode."
  (let ((lst (ac-ctags-get-signature-by-mode (substring-no-properties item)
                                             ac-ctags-tags-db
                                             'java-mode)))
    (cond
     ((= (length lst) 1) (car lst))
     ((> (length lst) 1)
      (reduce (lambda (x y) (concat x "\n" y)) lst))
     (t ac-ctags-no-document-message))))

(defun ac-ctags-action ()
  (cond
   ((eq major-mode 'c-mode)
    (ac-ctags-c-mode-action))
   (t
    nil)))

(defun ac-ctags-c-mode-action ()
  "Make and expand yasnippet if possible."
  (when (fboundp 'yas-expand-snippet)
    (let* ((cand (cdr ac-last-completion))
           (signature (get-text-property 0 'signature cand))
           (template (and (stringp signature)
                          (ac-ctags-make-yasnippet-template-from-signature
                           signature))))
      (when (stringp template)
        (delete-char (- (length signature)))
        (yas-expand-snippet template)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ac-source-ctags
(ac-define-source ctags
  '((candidates . ac-ctags-candidates)
    (candidate-face . ac-ctags-candidate-face)
    (selection-face . ac-ctags-selection-face)
    (document . ac-ctags-document)
    (requires . 2)
    (prefix . ac-ctags-prefix)
    (action . ac-ctags-action)))

(provide 'auto-complete-ctags)
;;; auto-complete-ctags.el ends here
