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
(require 'general)

(setq auto-insert-mode 't)

(setq global-whitespace-mode +1)
;; (setq-default show-trailing-whitespace t)
(defvar whitespace-style '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark))
(smart-tabs-mode +1)
(setq indent-tabs-mode nil)

;;; special modes
(general-add-hook 'special-mode-hook
                  '(hl-line-mode))

;;; prog modes
(general-add-hook 'prog-mode-hook
                  '(hl-line-mode hs-minor-mode flycheck-mode rainbow-delimiters-mode indent-guide-mode (lambda () (setq show-trailing-whitespace t))))
(general-add-hook '(c-mode-hook c++-mode-hook)
                  '(lsp))

;;; sexps
(general-add-hook '(closure-mode-hook elisp-mode-hook emacs-lisp-mode-hook lisp-mode-hook racket-mode-hook sexpy-mode-hook)
                  '(show-paren-mode visual-line-mode  electric-pair-mode electric-quote-mode electric-layout-mode))
;;; lisps
(general-add-hook '(closure-mode-hook elisp-mode-hook emacs-lisp-mode-hook lisp-mode-hook racket-mode-hook)
                  '(highlight-defined-mode))

;;; shells
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;;; text
(remove-hook 'text-mode-hook 'auto-fill-mode)
(general-add-hook 'text-mode-hook
		  '(visual-line-mode flyspell-mode))

;;; markdown
(remove-hook 'markdown-mode-hook 'auto-fill-mode)

(provide '+langs)
;;; +langs.el ends here
