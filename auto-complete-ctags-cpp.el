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

(defun ac-ctags-cpp-extract-class-name (line varname)
  "Extract a classname of VARNAME from LINE and return it if found, or nil."
  (let ((case-fold-search nil))
    (when (and (stringp line)
               (stringp varname)
               (string-match (concat "\\([A-Z][A-Za-z0-9_]+\\)\\(<[^ ]+>\\)?[ \t]+"
                                     varname)
                             line))
      (match-string-no-properties 1 line))))

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
          while (re-search-forward (concat "[ \t]" varname "[; =)]")
                                   end
                                   t)
          initially do (goto-char beg)
          when (ac-ctags-cpp-line-has-typeinfo-p
                varname
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position)))
          return (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position)))))

(defun ac-ctags-cpp-line-has-typeinfo-p (varname line)
  "Return t if this LINE contains type name, or nil."
  (let ((case-fold-search nil)
        (type-regexp (concat
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


(provide 'auto-complete-ctags-cpp)
;;; auto-complete-ctags-cpp.el ends here
