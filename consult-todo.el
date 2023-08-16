;;; consult-todo.el --- Search hl-todo keywords in consult -*- lexical-binding: t -*-

;; Copyright (C) 2023 liuyinz

;; Author: liuyinz <liuyinz@gmail.com>
;; Maintainer: liuyinz <liuyinz@gmail.com>
;; Created: 2021-10-03 03:44:36
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.3") (consult "0.35") (hl-todo "3.1.2"))
;; Homepage: https://github.com/liuyinz/consult-todo
;; License: GPL-3.0-or-later

;; This file is not a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide the command `consult-todo' to search and jump between hl-todo keywords.

;;; Code:

(require 'consult)
(require 'hl-todo)

(eval-when-compile (require 'pcase))

(defgroup consult-todo nil
  "Search hl-todo keywords in consult."
  :group 'consult-todo)

(defcustom consult-todo-narrow nil
  "Alist of (NARROW . KEYWORD) to display."
  :type '(repeat (cons (character :tag "Narrow")
                       (string :tag "Keyword")))
  :group 'consult-todo)

(defcustom consult-todo-narrow-other ?.
  "Charater used to narrow other keywords which aren't mapped."
  :type 'character
  :group 'consult-todo)

(defconst consult-todo--narrow
  '((?t . "TODO")
    (?f . "FIXME")
    (?b . "BUG")
    (?h . "HACK"))
  "Default mapping of narrow and keywords.")

(defun consult-todo--narrow-setup ()
  "Return narrow alist."
  (or consult-todo-narrow consult-todo--narrow))

(defun consult-todo--format (candidates)
  "Return formatted string according to CANDIDATES."
  (let (lst)
    (dotimes (i 3)
      (push (apply #'max
                   (mapcar (lambda (x)
                             (let ((elt (nth i x)))
                               (length (if (stringp elt) elt (number-to-string elt)))))
                           candidates))
            lst))
    (apply #'format "%%-%ds %%-%dd %%-%ds %%s" (reverse lst))))

(defun consult-todo--candidates ()
  "Return hl-todo keywards as alist."
  (when (member consult-todo-narrow-other (mapcar #'car (consult-todo--narrow-setup)))
    (error "Consult-todo: narrow keys repeat!"))
  (let* ((buffer (current-buffer))
         (regex (hl-todo--regexp))
         (candidates '()))
    (with-current-buffer buffer
      ;; (with-syntax-table hl-todo--syntax-table
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (let ((case-fold-search nil))
            (while (re-search-forward regex nil t)
              (let* ((end (match-end 0))
                     (type (match-string 0))
                     (type-1 (substring-no-properties type)))
                (push (list (buffer-name buffer)
                            (line-number-at-pos)
                            type
                            (match-beginning 0)
                            (match-end 0)
                            (string-trim (buffer-substring end (line-end-position)))
                            (or (car (rassoc type-1 (consult-todo--narrow-setup)))
                                consult-todo-narrow-other))
                      candidates)))))))
    ;; )
    (setq candidates (reverse candidates))
    (when candidates
      (let ((fmt (consult-todo--format candidates)))
        (mapcar
         (pcase-lambda (`(,buffer ,line ,type ,beg ,end ,msg ,narrow))
           (propertize (format fmt buffer line type msg)
                       'consult--candidate (list beg (cons 0 (- end beg)))
                       'consult--type narrow))
         candidates)))))

;;;###autoload
(defun consult-todo (&optional project)
  "Jump to hl-todo keywords.
If PROJECT is non-nil, then prompt with hl-todo keywords from all files in
PROJECT instead of the current buffer."
  (interactive "P")
  (consult--forbid-minibuffer)
  (consult--read
   ;; TODO add project functions
   (if project
       nil
     (consult-todo--candidates))
   :prompt "Hl-todo: "
   :category 'consult-todo
   :require-match t
   :sort nil
   :group (consult--type-group (consult-todo--narrow-setup))
   :narrow (consult--type-narrow (cons (cons consult-todo-narrow-other "Other")
                                       (consult-todo--narrow-setup)))
   :lookup #'consult--lookup-candidate
   :state (consult--jump-state)))

(provide 'consult-todo)
;;; consult-todo.el ends here
