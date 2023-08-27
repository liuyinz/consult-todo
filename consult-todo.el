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

(eval-when-compile (require 'cl-lib)
                   (require 'pcase))
(require 'consult)
(require 'hl-todo)

(defgroup consult-todo nil
  "Search hl-todo keywords in consult."
  :group 'consult-todo)

(defcustom consult-todo-narrow-alist nil
  "Alist of (NARROW . KEYWORD) to display."
  :type '(repeat (cons (character :tag "Narrow")
                       (string :tag "Keyword")))
  :group 'consult-todo)

(defcustom consult-todo-narrow-other ?.
  "Character used to narrow other keywords which aren't mapped."
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
  (or consult-todo-narrow-alist consult-todo--narrow))

(defun consult-todo--candidates (&optional buffers)
  "Return list of hl-todo keywords in current buffer.
If optional argument BUFFERS is non-nil, oprate on list of them."
  (cl-loop for buf in (or buffers (list (current-buffer)))
           append
           (with-current-buffer buf
             (save-excursion
               (save-restriction
                 (widen)
                 (goto-char (point-min))
                 (cl-loop while (re-search-forward (hl-todo--regexp) nil t)
                          with case-fold-search = nil
                          when (nth 4 (syntax-ppss))
                          collect
                          (list (buffer-name)
                                (line-number-at-pos)
                                (match-string 0)
                                (match-beginning 0)
                                (match-end 0)
                                (or (car (rassoc (match-string-no-properties 0)
                                                 (consult-todo--narrow-setup)))
                                    consult-todo-narrow-other)
                                (string-trim (buffer-substring-no-properties
                                              (point)
                                              (line-end-position))))))))))

(defun consult-todo--format (candidates)
  "Return formatted string according to CANDIDATES."
  (when candidates
    (mapcar
     (pcase-lambda (`(,buffer ,line ,type ,beg ,end ,narrow ,text))
       (propertize
        (format (apply #'format "%%-%ds %%-%dd  %%-%ds  %%s"
                       (cl-loop for i to 2
                                collect
                                (cl-loop for y in (mapcar (apply-partially #'nth i)
                                                          candidates)
                                         maximize
                                         (length (or (and (stringp y) y)
                                                     (number-to-string y))))))
                buffer line type text)
        'consult--candidate (list beg (cons 0 (- end beg)))
        'consult--type narrow))
     candidates)))

;;;###autoload
(defun consult-todo (&optional buffers)
  "Jump to hl-todo keywords in current buffer.
If BUFFERS is non-nil, prompt with hl-todo keywords in them instead."
  (interactive "P")
  (consult--forbid-minibuffer)
  (when (member consult-todo-narrow-other
                (mapcar #'car (consult-todo--narrow-setup)))
    (error "Consult-todo: narrow keys repeat!"))
  (consult--read
   (consult-todo--format (consult-todo--candidates buffers))
   :prompt "Hl-todo: "
   :category 'consult-todo
   :require-match t
   :sort nil
   :group (consult--type-group (consult-todo--narrow-setup))
   :narrow (consult--type-narrow (cons (cons consult-todo-narrow-other "Other")
                                       (consult-todo--narrow-setup)))
   :lookup #'consult--lookup-candidate
   :state (consult--jump-state)))

;;;###autoload
(defun consult-todo-all ()
  "Jump to hl-todo keywords in all live buffers."
  (interactive)
  (consult-todo (buffer-list)))

(provide 'consult-todo)
;;; consult-todo.el ends here
