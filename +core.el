;;; +core.el --- Modify built-in features.  -*- lexical-binding: t; -*-

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

;; Modify built-in emacs features.

;;; Code:

;; Windows
;; Fix annoying vertical window splitting.
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2015-08/msg00339.html

;; (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 50)))
;; (defun toggle-transparency ()
;;   (interactive)
;;   (let ((alpha (frame-parameter nil 'alpha)))
;;     (set-frame-parameter
;;      nil 'alpha
;;      (if (eql (cond ((numberp alpha) alpha)
;;                     ((numberp (cdr alpha)) (cdr alpha))
;;                     ;; Also handle undocumented (<active> <inactive>) form.
;;                     ((numberp (cadr alpha)) (cadr alpha)))
;;               100)
;;          '(85 . 50) '(100 . 100)))))
;; (global-set-key (kbd "C-c t") 'toggle-transparency)



(defcustom winblows--split-window-below nil
  "If non-nil, vertical splits produce new windows below."
  :group 'windows
  :type 'boolean)
(defcustom winblows--split-window-right nil
  "If non-nil, horizontal splits produce new windows to the right."
  :group 'windows
  :type 'boolean)
(defun split-window-sensibly (&optional window)
  (setq window (or window (selected-window)))
  (or (and (window-splittable-p window t)
           ;; Split window horizontally.
           (split-window window nil (if winblows--split-window-right 'left  'right)))
      (and (window-splittable-p window)
           ;; Split window vertically.
           (split-window window nil (if winblows--split-window-below 'above 'below)))
      (and (eq window (frame-root-window (window-frame window)))
           (not (window-minibuffer-p window))
           ;; If WINDOW is the only window on its frame and is not the
           ;; minibuffer window, try to split it horizontally disregarding the
           ;; value of `split-width-threshold'.
           (let ((split-width-threshold 0))
             (when (window-splittable-p window t)
               (split-window window nil (if winblows--split-window-right
                                            'left
                                          'right)))))))
(setq display-buffer-alist
      `(("\\(*\\(info\\|ansi-term\\|bitbake\\|docker\\|grep*\\|.*systemctl.*\\|Help\\|messages\\|help\\|helpful\\|trace-\\|Backtrace\\|Org\\|RefTeX.*\\)\\)"
         (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window display-buffer-pop-up-window)
         (side . right)
         (slot . 0)
         (window-width . 80))
        ;; ("\\*Async"
        ;;  (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window display-buffer-pop-up-window)
        ;;  (side . left)
        ;;  (slot . 0)
        ;;  (window-width . 80))
        ("\\*\\(remember\\|Async\\)"
         (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window display-buffer-pop-up-window)
         (side . bottom)
         (slot . 0)
         (window-height . 10))
        ;; Split shells at the bottom
        ("^\\*e?shell"
         (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-below-selected)
         (window-min-height . 20))
        ("\\(magit:\\|pydoc\\)"
         (display-buffer-reuse-window display-buffer-same-window))
        ;; ("\\*"
        ;;  (display-buffer-reuse-window display-buffer-pop-up-window))
        ))
(add-to-list 'display-buffer-alist
             '("*scratch"
               (display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)
               (window-height . 10)))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; (setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setf shell-file-name "/bin/zsh")

(setq custom-file "~/.emacs.d/overdr0ne/customize.el")
;; (general-add-hook 'kill-emacs-hook
;;                   '(customize-save-customized))

(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(save-place-mode +1)

(blink-cursor-mode -1)

(tool-bar-mode -1)

(scroll-bar-mode -1)

(column-number-mode)

(menu-bar-mode -1)

;; use what-cursor-position to determine font
(setq sam-font
      "-CTDB-FiraMono Nerd Font-bold-normal-normal-*-16-*-*-*-m-0-iso10646-1")
(add-to-list 'default-frame-alist '(alpha . (93 . 70)))
(add-to-list 'default-frame-alist `(font . ,sam-font))
(set-frame-parameter (selected-frame) 'alpha '(93 . 70))

(setq tags-add-tables nil)
(setq large-file-warning-threshold nil)
(set-face-attribute 'default nil :height 140)
;; (setq display-line-numbers-type nil)
(setq user-mail-address "scmorris.dev@gmail.com")
(turn-off-auto-fill)
(auto-fill-mode -1)
(setq overdr0ne-directory (concat user-emacs-directory "overdr0ne"))

(global-auto-revert-mode +1)

(setq browse-url-browser-function 'eww-browse-url)

(setq load-prefer-newer t)

(setq inhibit-startup-screen t)

(setq debug-on-error t)

(setf scroll-step 1)

(setf ring-bell-function 'ignore)

(setq-default major-mode 'text-mode)

(setf global-so-long-mode t)

(setq-default tab-width 2)

(setq-default indent-tabs-mode nil)

(setq delete-pair-blink-delay .15)

;; (set-frame-font "-UKWN-Mononoki Nerd Font Mono-bold-normal-normal-*-15-*-*-*-m-0-iso10646-1")

;; (desktop-save-mode 1)

(setq initial-scratch-message nil)

(provide '+core)
;;; +core.el ends here
