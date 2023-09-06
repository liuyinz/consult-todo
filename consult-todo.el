;;; consult-todo.el --- Search hl-todo keywords in consult -*- lexical-binding: t -*-

;; Copyright (C) 2023 liuyinz

;; Author: liuyinz <liuyinz@gmail.com>
;; Maintainer: liuyinz <liuyinz@gmail.com>
;; Created: 2021-10-03 03:44:36
;; Version: 0.2.0
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

;; Provide commands `consult-todo' to search, filter, jump to hl-todo keywords.

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

(defcustom consult-todo-other (cons ?. "OTHER")
  "Cons mapping for narrow and missing keywords."
  :type '(cons character string)
  :group 'consult-todo)

(defcustom consult-todo-only-comment nil
  "If non-nil, only search todo keywords in comments."
  :type 'boolean
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
  (or consult-todo--narrow-extend
      (if-let* (((consp consult-todo-other))
                (narrow (car consult-todo-other))
                (group (cdr consult-todo-other))
                ((and (characterp narrow) (not (assoc narrow (consult-todo--narrow)))))
                ((and (stringp group) (not (rassoc group (consult-todo--narrow))))))
          (setq consult-todo--narrow-extend
                (cons consult-todo-other (consult-todo--narrow)))
        (user-error "Consult-todo-other: format error or conflicts with consult-todo-narrow")
        (setq consult-todo--narrow-extend nil))))

(defun consult-todo--candidates (&optional buffers)
  "Return list of hl-todo keywords in current buffer.
If optional argument BUFFERS is non-nil, operate on list of them."
  (cl-loop for buf in (or buffers (list (current-buffer)))
           ;; with gc-cons-threshold = most-positive-fixnum
           ;; with gc-cons-percentage = 1.0
           append
           (with-current-buffer buf
             (save-excursion
               (save-restriction
                 (widen)
                 (goto-char (point-min))
                 (cl-loop while (hl-todo--search)
                          when (or (null consult-todo-only-comment)
                                   (nth 4 (syntax-ppss)))
                          collect
                          ;; HACK when buffer is too large, match-string return nil
                          ;; in one match, use thing-at-point to get the match-string
                          ;; as type. At this failed match, point is still accurate,
                          ;; match-begin and match-end is limit to accessible portion
                          ;; of buffer
                          (let ((type (or (match-string-no-properties 2)
                                          (save-excursion
                                            (backward-to-word)
                                            (substring-no-properties
                                             (save-match-data
                                               (thing-at-point 'word)))))))
                            (list (buffer-name)
                                  (number-to-string (line-number-at-pos))
                                  type
                                  (copy-marker (point))
                                  (car (or (rassoc type (consult-todo--narrow))
                                           consult-todo-other))
                                  (string-trim (buffer-substring-no-properties
                                                (point)
                                                (line-end-position)))))))))))

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
