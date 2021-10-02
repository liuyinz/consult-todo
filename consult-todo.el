;;; consult-todo.el --- Provide the command `consult-todo' -*- lexical-binding: t -*-

;; Copyright (C) 2021 liuyinz

;; Author: liuyinz <liuyinz@gmail.com>
;; Maintainer: liuyinz <liuyinz@gmail.com>
;; Created: 2021-10-03 03:44:36
;; Version: 0.1.0
;; Package-Requires: ((emacs "27") (consult "0.9") (hl-todo "3.1.2"))
;; Homepage: https://github.com/liuyinz/consult-todo
;; License: GPL-3.0-or-later

;; This file is not a part of GNU Emacsl.

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

;;; Code:

(require 'consult)
(require 'hl-todo)

(defconst consult-todo--narrow
  '((?c . "Current")
    (?b . "Buffers")
    (?p . "Project")))

(defun consult-todo--candidates ()
  "Return hl-todo keywards as alist."
  (consult--forbid-minibuffer)
  (let* ())
)

(provide 'consult-todo)
;;; consult-todo.el ends here
