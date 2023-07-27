;;; +langs.el --- personal lang-specific configuration    -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Sam

;; Author: Sam;;; ~/.emacs.d/+use-package.el -*- lexical-binding: t; -*- <scmorris.dev@gmail.com>
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

;; Set up hooks for major-modes.

;;; Code:
(require '+modules)

(setq auto-insert-mode 't)

(setq global-whitespace-mode +1)
;; (setq-default show-trailing-whitespace t)
(defvar whitespace-style
  '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark))
;; (smart-tabs-mode nil)
;; (setq indent-tabs-mode nil)

;;; special modes
;; (add-hook 'special-mode-hook 'enable-hl-line-mode)
;; (general-add-hook 'special-mode-hook
;;                   '(hl-line-mode))

;;; conf modes
;; (add-hook 'conf-mode-hook
;;                   '(hl-line-mode))

;;; prog modes
(defun sam-set-cape-funcs ()
  (setq completion-at-point-functions
        (setq completion-at-point-functions
              '(cape-file cape-line cape-keyword cape-dict cape-dabbrev t)))
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  )
(dolist (fun
         '( highlight-defined-mode  hs-minor-mode sam-set-cape-funcs rainbow-delimiters-mode indent-guide-mode
            (lambda ()
              (setq-local show-trailing-whitespace t))))
  (dolist (hook '(prog-mode-hook conf-mode-hook))
    (add-hook hook fun)))

(dolist (fun '(electric-pair-mode electric-quote-mode electric-layout-mode))
  (add-hook 'prog-mode-hook fun))

(dolist (fun '( indent-tabs-mode
                (lambda ()
                  (setq-local tab-width 8))))
  (dolist (hook '(c-mode-hook c++-mode-hook dts-mode-hook))
    (add-hook hook fun)))

;;; evim
(dolist (hook '(bitbake-mode-hook))
  (lambda ()
    (setq-local tab-width 4)))

;;; evim
(dolist (hook '( diff-mode-hook text-mode-hook prog-mode-hook conf-mode-hook Info-mode-hook helpful-mode-hook bitbake-mode-hook extempore-mode-hook))
  (add-hook hook #'evim-normal-mode))

;;; sexps
(dolist (hook '(closure-mode-hook elisp-mode-hook emacs-lisp-mode-hook lisp-mode-hook racket-mode-hook sexpy-mode-hook))
  (dolist (fun '( show-paren-mode visual-line-mode electric-pair-mode electric-quote-mode electric-layout-mode
                  (lambda ()
                    (add-to-list 'completion-at-point-functions
                                 #'elisp-completion-at-point)
                    (setq-local tab-width 2))))
    (add-hook hook fun)))
;; (general-add-hook '(closure-mode-hook elisp-mode-hook emacs-lisp-mode-hook lisp-mode-hook racket-mode-hook sexpy-mode-hook)
;;                  '(show-paren-mode visual-line-mode  electric-pair-mode electric-quote-mode electric-layout-mode
;;            (lambda () (add-to-list 'completion-at-point-functions
;;                  #'elisp-completion-at-point))))
;;; lisps
;; (dolist ('(closure-mode-hook elisp-mode-hook emacs-lisp-mode-hook lisp-mode-hook racket-mode-hook))
;;                   '(highlight-defined-mode))

;;; shells
(dolist (hook '(shell-mode-hook))
  (dolist (fun '(ansi-color-for-comint-mode-on evim-insert-term-mode))
    (add-hook hook fun)))
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;;; terms
;; (add-hook 'term-mode-hook 'evim-insert-term-mode)

;;; text
(remove-hook 'text-mode-hook 'auto-fill-mode)
(dolist (fun '(visual-line-mode flyspell-mode))
  (add-hook 'text-mode-hook fun))

(dolist (fun '(visual-line-mode))
  (add-hook 'dired-mode-hook fun))

;;; markdown
(remove-hook 'markdown-mode-hook 'auto-fill-mode)


;; styles
(require 'cc-styles)

;;; Linux kernel
(defun linux-kernel-coding-style/c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

;; Add Linux kernel style
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-add-style "linux-kernel"
			 '("linux" (c-offsets-alist
				    (arglist-cont-nonempty
				     c-lineup-gcc-asm-reg
				     linux-kernel-coding-style/c-lineup-arglist-tabs-only))))))

(defun linux-kernel-coding-style/setup ()
  (let ((filename (buffer-file-name)))
    ;; Enable kernel mode for the appropriate files
    (when (and filename
	       (or (locate-dominating-file filename "Kbuild")
		   (locate-dominating-file filename "Kconfig")
		   (save-excursion (goto-char 0)
				   (search-forward-regexp "^#include <linux/\\(module\\|kernel\\)\\.h>$" nil t))))
      (setq indent-tabs-mode t)
      (setq tab-width 8)
      (setq c-basic-offset 8)
      (c-set-style "linux-kernel")
      (message "Setting up indentation for the linux kernel"))))

(add-hook 'c-mode-hook 'linux-kernel-coding-style/setup)

;;; Python
(setq-default python-indent-offset 4)

(provide '+langs)
;;; +langs.el ends here
