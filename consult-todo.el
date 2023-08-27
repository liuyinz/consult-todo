;;; consult-todo.el --- Search hl-todo keywords in consult -*- lexical-binding: t -*-

;; Copyright (C) 2023 liuyinz

;; Author: liuyinz <liuyinz@gmail.com>
;; Maintainer: liuyinz <liuyinz@gmail.com>
;; Created: 2021-10-03 03:44:36
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (consult "0.35") (hl-todo "3.1.2"))
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

(defcustom consult-todo-narrow nil
  "Alist of (NARROW . KEYWORD) to display."
  :type '(repeat (cons (character :tag "Narrow")
                       (string :tag "Keyword")))
  :group 'consult-todo)

(defcustom consult-todo-other ?.
  "Character used to narrow other keywords which aren't mapped."
  :type 'character
  :group 'consult-todo)

(defconst consult-todo--narrow
  '((?t . "TODO")
    (?f . "FIXME")
    (?b . "BUG")
    (?h . "HACK"))
  "Default mapping of narrow and keywords.")

(defvar consult-todo--narrow-extend nil
  "Default mapping of narrow and keywords include OTHER if exists.")

(defun consult-todo--narrow ()
  "Return narrow alist."
  (or consult-todo-narrow consult-todo--narrow))

(defun consult-todo--narrow-extend ()
  "Return narrow alist include `consult-todo-other' if it's non-nil."
  (when (and consult-todo-other
             (or (not (characterp consult-todo-other))
                 (memq consult-todo-other (mapcar #'car (consult-todo--narrow)))))
    (user-error "Consult-todo-other: key '%s' is not char or conflicts with other keys"
                (single-key-description consult-todo-other)))
  (or consult-todo--narrow-extend
      (setq consult-todo--narrow-extend
            (if (null consult-todo-other)
                consult-todo--narrow
              (cons (cons consult-todo-other "OTHER")
                    (consult-todo--narrow))))))

(defun consult-todo--candidates (&optional buffers)
  "Return list of hl-todo keywords in current buffer.
If optional argument BUFFERS is non-nil, operate on list of them."
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
                                (number-to-string (line-number-at-pos))
                                (match-string-no-properties 0)
                                (copy-marker (match-beginning 0))
                                (or (car (rassoc (match-string-no-properties 0)
                                                 (consult-todo--narrow)))
                                    consult-todo-other)
                                (string-trim (buffer-substring-no-properties
                                              (point)
                                              (line-end-position))))))))))

(defun consult-todo--format (candidates)
  "Return formatted string according to CANDIDATES."
  (if candidates
      (mapcar
       (pcase-lambda (`(,buffer ,line ,type ,marker ,narrow ,text))
         (propertize
          (format (apply #'format "%%-%ds %%-%ds %%-%ds %%s"
                         (cl-loop for i to 2
                                  collect (seq-max (mapcar
                                                    (lambda(x) (length (nth i x)))
                                                    candidates))))
                  (propertize buffer 'face 'consult-file)
                  (propertize line   'face 'consult-line-number)
                  ;; WONTFIX don't support regexp keywords face
                  (propertize type   'face (hl-todo--combine-face
                                            (cdr (assoc type hl-todo-keyword-faces))))
                  text)
          'consult-location (cons marker line)
          'consult--type narrow))
       candidates)
    (user-error "No hl-todo keywords")))

;;;###autoload
(defun consult-todo (&optional buffers)
  "Jump to hl-todo keywords in current buffer.
If BUFFERS is non-nil, prompt with hl-todo keywords in them instead."
  (interactive "P")
  (consult--forbid-minibuffer)
  (consult--read
   (consult-todo--format (consult-todo--candidates buffers))
   :prompt "Go to hl-todo: "
   :category 'consult-location
   :require-match t
   :sort nil
   :group (consult--type-group (consult-todo--narrow-extend))
   :narrow (consult--type-narrow (consult-todo--narrow-extend))
   :lookup #'consult--lookup-location
   :state (consult--jump-state)))

;;;###autoload
(defun consult-todo-all ()
  "Jump to hl-todo keywords in all live buffers."
  (interactive)
  (consult-todo (buffer-list)))

(provide 'consult-todo)
;;; consult-todo.el ends here
