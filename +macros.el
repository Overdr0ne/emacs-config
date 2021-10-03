;;; +macros.el --- Personal macros. -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Sam

;; Author: Sam
;; Keywords: local

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

;; Define personal macros.

;;; Code:

(defun deftoggle-var-doc (name)
  (concat "Non-nil if " name " is enabled.\n\n"
          "See " name
          " command for a description of this toggle."))
(defun deftoggle-fun-doc (name doc)
  (concat "Toggle " name " on or off.\n\n" doc
          "\n\nIf called interactively, enable Global Gumshoe-Buf mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is toggle; disable the mode otherwise.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, toggle toggles the state."))
(defmacro deftoggle (name doc enabler disabler)
  `(progn
     (defvar ,name nil ,(deftoggle-var-doc (symbol-name name)))
     (defun ,name (&optional enable)
       ,(deftoggle-fun-doc (symbol-name name) doc)
       (interactive)
       (if (called-interactively-p 'interactive)
           (progn
             (if ,name
                 ,disabler
               ,enabler)
             (setq ,name (not ,name)))
         (progn
           (if enable
               ,enabler
             ,disabler)
           (setq ,name enable))))))

(defmacro define-move-and-insert
    (name &rest body)
  `(defun ,name (count &optional vcount skip-empty-lines)
     ;; Following interactive form taken from the source for `evil-insert'
     (interactive
      (list (prefix-numeric-value current-prefix-arg)
            (and (evil-visual-state-p)
                 (memq (evil-visual-type) '(line block))
                 (save-excursion
                   (let ((m (mark)))
                     ;; go to upper-left corner temporarily so
                     ;; `count-lines' yields accurate results
                     (evil-visual-rotate 'upper-left)
                     (prog1 (count-lines evil-visual-beginning evil-visual-end)
                       (set-mark m)))))
            (evil-visual-state-p)))
     (atomic-change-group
       ,@body
       (evil-insert count vcount skip-empty-lines))))

(provide '+macros)
;;; +macros.el ends here
