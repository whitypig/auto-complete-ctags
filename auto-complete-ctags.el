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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ac-ctags-visit-tags-file ()
  "Select tags file."
  (interactive)
  (let ((tagsfile (read-file-name "Visit tags file (default tags): "
                                   nil
                                   "tags"
                                   t))
        (tagslist nil))
    (when (ac-ctags-is-valid-tags-file-p tagsfile)
      ;; ask user whether the tags will be inserted into the current
      ;; list or new one.
      (setq tagslist (if (ac-ctags-create-new-list-p tagsfile)
                         (ac-ctags-insert-into-new-list tagsfile)
                       (ac-ctags-insert-into-current-list tagsfile)))
      ;; Either way, we have to (re)build completion table.
      (and (not (null tagslist))
           (listp tagslist)
           (ac-ctags-build-completion-table tagslist))
      ;; Then update the current tags list.
      (setq ac-ctags-current-tags-list tagslist))))

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

(defun ac-ctags-build-completion-table (tagslist)
  "Build completion table from TAGSLIST."
  (loop for tags in tagsfile
        collect (ac-ctags-build-completion-table-from-tags tags)))

(defun ac-ctags-build-completion-table-from-tags (tags)
  "Extract tag information for each entry in TAGS and return them
  as a list."
  )

(provide 'auto-complete-ctags)
;;; auto-complete-ctags.el ends here
