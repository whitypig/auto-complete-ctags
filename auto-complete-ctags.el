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
(eval-when-compile
  (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup auto-complete-ctags nil
  "A source for auto-complete-mode usign Exuberant ctags."
  :prefix "ac-ctags-"
  :group 'convenience)

(defcustom ac-ctags-candidate-limit 50
  "The upper limit number of candidates to be shown."
  :type 'number
  :group 'auto-complete-ctags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ac-ctags-current-tags-list nil
  "Current list of tags.")

(defvar ac-ctags-tags-list-set nil
  "The set of lists of tags files.")

(defvar ac-ctags-tags-db nil
  "A list of list each of which is `(name command signature)'")

(defvar ac-ctags-completion-table nil
  "A list of names which are extracted from tags in
  `ac-ctags-current-tags-list'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ac-ctags-visit-tags-file ()
  "Select tags file."
  (interactive)
  (let ((tagsfile (expand-file-name
                   (read-file-name "Visit tags file (default tags): "
                                   nil
                                   "tags"
                                   t)))
        (tagslist nil))
    (unless (ac-ctags-is-valid-tags-file-p tagsfile)
      (error "Invalid tags: %s is not a valid tags file" tagsfile))
    ;; ask user whether the tags will be inserted into the current
    ;; list or a new one, and do insert.
    (setq tagslist (if (ac-ctags-create-new-list-p tagsfile)
                       (ac-ctags-insert-into-new-list tagsfile)
                     (ac-ctags-insert-into-current-list tagsfile)))
    ;; Either way, we have to (re)build completion table.
    (and (not (null tagslist))
         (listp tagslist)
         (ac-ctags-build-completion-table tagslist))
    ;; Finally, update the current tags list.
    (setq ac-ctags-current-tags-list tagslist)))

(defun ac-ctags-create-new-list-p (tagsfile)
  "Ask user whether to create the new tags file list or use the
current one. TAGSFILE is guaranteed to be a valid tagfile."
  ;; Check if TAGSFILE is already in the current list.
  (if (member tagsfile ac-ctags-current-tags-list)
      (y-or-n-p "The tags file is already in the current tags list.\nAnyway create new list? ")
    ;; If not in the list, ask the user what to do.
    (y-or-n-p "Create new tags list? ")))

(defun ac-ctags-insert-tags-into-new-list (tagsfile)
  "Insert TAGSFILE into a new tags list."
  (setq ac-ctags-current-tags-list (list tagsfile))
  (unless (member ac-ctags-current-tags-list
                  ac-ctags-tags-list-set)
    (push ac-ctags-current-tags-list ac-ctags-tags-list-set)))

(defun ac-ctags-insert-tags-into-current-list (tagsfile)
  "Insert TAGSFILE into the current tags list."
  (setq ac-ctags-tags-list-set
        (delete ac-ctags-current-tags-list ac-ctags-tags-list-set))
  (push tagsfile ac-ctags-current-tags-list)
  (push ac-ctags-current-tags-list ac-ctags-tags-list-set))

(defun ac-ctags-select-tags-list ()
  "Swith to another list of tags."
  (interactive))

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

(defun ac-ctags-build-tagdb (tagslist)
  "Build tagdb from each element of TAGSLIST."
  (setq ac-ctags-tags-db
        (mapcan #'ac-ctags-build-tagdb-from-tags
                tagslist)))

(defun ac-ctags-build-tagdb-from-tags (tags)
  "Build tag information db frm TAGS and return the db.
Each element of DB is a list like (name cmd signature) where NAME
  is tag name, CMD is info constructed from EX command, and
  SIGNATURE is as is. If NAME entry has no signature, then
  SIGNATURE is nil.
TAGS is expected to be an absolute path name."
  (assert (ac-ctags-is-valid-tags-file-p tags))
  (let ((db nil))
    (with-temp-buffer
      (insert-file-contents-literally tags)
      ;; todo: How can we get the return type? `signature' in tags file
      ;; does not contain the return type.
      (while (re-search-forward
              "^\\([^\t]+\\)\t[^\t]+\t/^\\([^\\$;{}]+\\)[^\t]+\\$/;\"\t.*$"
              nil t)
        (let (line name cmd signature)
          (setq line (match-string-no-properties 0)
                name (match-string-no-properties 1)
                cmd (match-string-no-properties 2))
          ;; If this line contains a signature, we get it.
          (when (string-match "signature:\\([^\t\n]+\\)" line)
            (setq signature (match-string-no-properties 1 line)))
          (push (list name (ac-ctags-trim-whitespace cmd) signature) db))))
    db))

(defun ac-ctags-build-completion-table (tagsdb)
  (setq ac-ctags-completion-table
        (sort (mapcar #'car tagsdb) #'string<)))

(defun ac-ctags-trim-whitespace (str)
  "Trim prepending and trailing whitespaces and return the result
  string."
  (replace-regexp-in-string "[ \t]+$" ""
                            (replace-regexp-in-string "^[ \t]+" "" str)))

;; todo: more accurate signatures are desirable.
;; i.e. not `(double d)' but `void func(double d) const',
;; but for now jsut return signature entry in tags.
(defun ac-ctags-get-signature (name db)
  "Return a list of signatures corresponding NAME."
  (loop for e in db
        when (and (string= name (car e))
                  (not (null (caddr e))))
        collect (caddr e)))
                  

(provide 'auto-complete-ctags)
;;; auto-complete-ctags.el ends here
