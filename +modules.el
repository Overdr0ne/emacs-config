;; +modules.el --- personal module configuration    -*- lexical-binding: t; -*-

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

;; Set up my modules.

;;; Code:

;;(require 'use-package)
;;(require 'straight)
;;(require 'emacs)

(use-package loaddefs
  :straight (loaddefs :type built-in)
  :config
  (setopt global-so-long-mode t)
  )

(progn ;; startup.el does not provide startup
  (setopt initial-scratch-message nil)
  (setopt inhibit-startup-screen t)
  (turn-off-auto-fill)
  (auto-fill-mode -1)
  )

(use-package browse-url
  :straight (browse-url :type built-in)
  :config
  ;; (setopt browse-url-browser-function 'eww-browse-url)
  (setopt browse-url-firefox-program "firefox")
  (setopt browse-url-browser-function 'browse-url-firefox)
  )

;; (use-package pixel-mode
;;   :straight (pixel-mode :type built-in)
;;   :config
;;   (setopt pixel-scroll-precision-use-momentum t)
;;   )

(use-package autorevert
  :straight (autorevert :type built-in)
  :config
  )

(progn ;; lisp.el does not provide lisp
  (setopt delete-pair-blink-delay .15)
  )

(use-package skey
  :straight (skey :local-repo "~/src/skey")
  ;;   :straight (skey :type git
  ;;                   :host github
  ;;                   :repo "Overdr0ne/skey"
  ;;                   :branch "main"))
  )


(use-package seq)
(use-package let-alist)
;; (use-package pkg-info)
(use-package dash)

(use-package cus-edit
  :after (wid-edit)
  :straight (cus-edit :type built-in
                      ;; :build (:not compile)
                      ))

(use-package easymenu
  :straight (easymenu :type built-in
                      ;;                       :build (:not compile)
                      ))

(use-package tramp
  :straight (tramp :type built-in)
  :config
  (setopt tramp-verbose 6)
  ;; (defun add-ssh-agent-to-tramp ()
  ;;   (cl-pushnew '("-A")
  ;;               (cadr (assoc 'tramp-login-args
  ;;                                       ; if on Windows using Putty with Pageant,
  ;;                                       ; replace "ssh" with "plink"
  ;;                            (assoc "ssh" tramp-methods)))
  ;;               :test #'equal))
  ;; (add-ssh-agent-to-tramp)
  ;; :config
  ;; (setopt tramp-remote-path
  ;;       (append '("~/.guix-home/profile/bin"
  ;;                 "~/.guix-home/profile/sbin"
  ;;                 "~/.guix-profile/bin"
  ;;                 "~/.guix-profile/sbin")
  ;;             tramp-remote-path))
  )

(use-package lsp-mode
  ;; :hook
  ;; ((python-mode
  ;;   js-mode
  ;;   typescript-mode
  ;;   java-mode
  ;;   c-mode
  ;;   c++-mode
  ;;   go-mode) . lsp-deferred) ; lsp-deferred starts the server only when needed
  ;; :commands (lsp lsp-deferred)
  )

(use-package lsp-scheme
  :config
  ;; (add-hook 'scheme-mode-hook #'lsp-scheme)

  ;; (setopt lsp-scheme-implementation "guile")
  )

(use-package subword
  :straight (subword :type built-in)
  :hook ((python-mode) . subword-mode))

;; (use-package aggressive-indent-mode)

(use-package frame
  :straight (frame :type built-in)
  :init
  (setopt window-divider-default-right-width 2)
  (setopt window-divider-default-bottom-width 2)
  (setopt window-divider-default-places t)
  (set-face-attribute 'window-divider nil
                      :foreground nil
                      :inherit 'minibuffer-prompt))

(use-package window
  :straight (window :type built-in)
  :config
  (setopt window-min-height 1)
  ;; Maximum number of side-windows to create on (left top right bottom)
  (setopt window-sides-slots '(1 1 1 1))

  ;; redefine min-size func to not factor in window-divider into
  ;; min window size.
  (defun window--min-size-1 (window horizontal ignore pixelwise)
    "Internal function of `window-min-size'."
    (let ((sub (window-child window)))
      (if sub
          (let ((value 0))
            ;; WINDOW is an internal window.
            (if (window-combined-p sub horizontal)
                ;; The minimum size of an iso-combination is the sum of
                ;; the minimum sizes of its child windows.
                (while sub
                  (setq value (+ value
                                 (window--min-size-1
                                  sub horizontal ignore pixelwise)))
                  (setq sub (window-right sub)))
              ;; The minimum size of an ortho-combination is the maximum
              ;; of the minimum sizes of its child windows.
              (while sub
                (setq value (max value
                                 (window--min-size-1
                                  sub horizontal ignore pixelwise)))
                (setq sub (window-right sub))))
            value)
        (with-current-buffer (window-buffer window)
          (cond
           ((window-size-fixed-p window horizontal ignore)
            ;; The minimum size of a fixed size window is its size.
            (window-size window horizontal pixelwise))
           ((eq ignore 'safe)
            ;; If IGNORE equals `safe' return the safe value.
            (window-safe-min-size window horizontal pixelwise))
           (horizontal
            ;; For the minimum width of a window take fringes and
            ;; scroll-bars into account.  This is questionable and should
            ;; be removed as soon as we are able to split (and resize)
            ;; windows such that the new (or resized) windows can get a
            ;; size less than the user-specified `window-min-height' and
            ;; `window-min-width'.
            (let* ((char-size (frame-char-size window t))
                   (fringes (window-fringes window))
                   (margins (window-margins window))
                   ;; Let the 'min-margins' parameter override the actual
                   ;; widths of the margins.  We allow any number to
                   ;; replace the values specified by `window-margins'.
                   ;; See bug#24193 for the rationale of this parameter.
                   (min-margins (window-parameter window 'min-margins))
                   (left-min-margin (and min-margins
                                         (numberp (car min-margins))
                                         (car min-margins)))
                   (right-min-margin (and min-margins
                                          (numberp (cdr min-margins))
                                          (cdr min-margins)))
                   (pixel-width
                    (+ (window-safe-min-size window t t)
                       (* (or left-min-margin (car margins) 0) char-size)
                       (* (or right-min-margin(cdr margins) 0) char-size)
                       (car fringes) (cadr fringes)
                       (window-scroll-bar-width window)
                       (window-right-divider-width window))))
              (if pixelwise
                  (max
                   (if window-resize-pixelwise
                       pixel-width
                     ;; Round up to next integral of columns.
                     (* (ceiling pixel-width char-size) char-size))
                   (if (window--min-size-ignore-p window ignore)
                       0
                     (window-min-pixel-width window)))
                (max
                 (ceiling pixel-width char-size)
                 (if (window--min-size-ignore-p window ignore)
                     0
                   window-min-width)))))
           ((let ((char-size (frame-char-size window))
                  (pixel-height
                   (+ (window-safe-min-size window nil t)
                      (window-tab-line-height window)
                      (window-header-line-height window)
                      (window-scroll-bar-height window)
                      (window-mode-line-height window)
                      ;; (window-bottom-divider-width window)
                      )))
              (if pixelwise
                  (max
                   (if window-resize-pixelwise
                       pixel-height
                     ;; Round up to next integral of lines.
                     (* (ceiling pixel-height char-size) char-size))
                   (if (window--min-size-ignore-p window ignore)
                       0
                     (window-min-pixel-height window)))
                (max (ceiling pixel-height char-size)
                     (if (window--min-size-ignore-p window ignore)
                         0
                       window-min-height)))))))))))

(use-package text-property-search
  :straight (text-property-search :type built-in)
  )

;; Affecting performance
;; (use-package hyperbole
;;   :init
;;   (setopt hkey-init nil))

(use-package rx
  :straight (rx :type built-in))

(use-package mb-depth
  :straight (mb-depth :type built-in)
  )

(use-package select
  :straight (select :type built-in)
  :config
  (setopt select-enable-primary t))

(use-package sh-script
  :straight (sh-script :type built-in)
  :config
  (add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode)))

(use-package typescript-ts-mode
  :straight (typescript-ts-mode :type built-in)
  :init
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))

;; (use-package c-ts-mode
;;   :straight (c-ts-mode :type built-in)
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode)))

(use-package simple
  :straight (simple :type built-in)
  :config
  (setopt indent-tabs-mode nil)
  (setopt kill-ring-max 1000)
  ;; :config
  (add-to-list 'auto-mode-alist '("*Shell Command Output*\\'" . text-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.Async Shell Command\\'" . text-mode))
  ;; weird hack to prevent spurious appends to kill ring
  ;; why in the hell is that the default behaviour??
  ;; TODO: it shouldn’t be appending when I move the cursor between kills...
  (advice-add 'kill-whole-line :around
            (lambda (fn &rest args)
              (let (last-command)
                (push "" kill-ring)
                (apply fn args))))
  )

(use-package view
  :straight (view :type built-in)
  ;; :config
  ;; (add-to-list 'auto-mode-alist '("\\.Shell Command\\'" . view-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.Async Shell Command\\'" . view-mode))
  )

(use-package logview
  :config
  (add-to-list 'auto-mode-alist '("\\.log\\'" . logview-mode))
  (add-to-list 'auto-mode-alist '(".*/var/log.*" . logview-mode))
  )

(use-package term
  :straight (term :type built-in)
  :config
  (defmacro term-pager-enabled () nil)
  (setopt term-pager-count nil)
  ;; (setopt term-suppress-hard-newline t)
  ;; (defun term--unwrap-visible-long-lines (width) nil)
  )

(use-package project
  :straight (project :type built-in)
  :config
  (setopt project-vc-merge-submodules nil)
  (setopt project-switch-commands
          '((find-file "Find file" ?f)
            (project-find-regexp "Find regexp")
            (project-find-dir "Find directory")
            (magit-status "Magit")
            (project-eshell "Eshell"))))

(use-package wid-edit
  :straight (wid-edit :type built-in
                      ))

(use-package tab-bar
  :straight (tab-bar :type built-in
                     )
  :config
  (setopt tab-bar-separator "|")
  )
(use-package savehist
  :straight (savehist :type built-in)
  :config
  (setopt savehist-additional-variables
          (append savehist-additional-variables
                  '(kill-ring search-ring regexp-search-ring compile-history log-edit-comment-ring)))
  ;; (add-to-list 'savehist-additional-variables '(kill-ring search-ring regexp-search-ring compile-history log-edit-comment-ring))
  )

(use-package cl-lib
  :straight (cl-lib :type built-in))

(use-package transient
  ;; :straight (transient :build (:not compile))
  ;;  :after (compat)
  )

;;(use-package sql)

(use-package posframe)

(use-package helpful)

(use-package winner
  :straight (winner :type built-in))

;; (use-package icomplete
;;   :init
;;   (setopt icomplete-show-matches-on-no-input t)
;;   (setopt icomplete-vertical-mode +1))

(use-package minibuffer
  :straight (minibuffer :type built-in)
  :config
  ;; (setopt completions-sort 'historical)
  )
(use-package vertico
  :straight (vertico
             :files ("*.el" "extensions/*.el"))
  :init
  ;; (setopt vertico-sort-function nil)
  (setopt vertico-sort-function 'vertico-sort-history-length-alpha)
  (setopt completion-ignored-extensions nil)
  ;; :config

  ;; Configure the display per command.
  ;; Use a buffer with indices for imenu
  ;; and a flat (Ido-like) menu for M-x.
  (setopt vertico-multiform-commands
          '((sam-insert-zsh-command (vertico-sort-function . nil))))
  ;;         ;; (gumshoe-peruse-globally (vertico-sort-function . nil))
  ;;         ;; (gumshoe-peruse-in-buffer (vertico-sort-function . nil))
  ;;         ;; (gumshoe-peruse-in-window (vertico-sort-function . nil))
  ;;         ;; (consult-imenu buffer indexed)
  ;;         ;; (persp-switch-to-buffer* (vertico-sort-function . vertico-sort-history-alpha))
  ;;         ))

  ;; (advice-add #'vertico--sort-function :before-until #'completion-category-sort-function)

  ;; (setopt vertico-multiform-categories
  ;;       '(
  ;;         ;; (file grid)
  ;;         ;; (symbol (vertico-sort-function . vertico-sort-alpha))
  ;;         ;; (file (vertico-sort-function . sort-directories-first))
  ;;         (consult-grep buffer)))

  ;; Sort directories before files
  ;; (defun sort-directories-first (files)
  ;;   (setopt files (vertico-sort-history-length-alpha files))
  ;;   (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
  ;;          (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))
  )
(use-package vertico-posframe)
(use-package orderless
  :config
  (setopt completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion)))))

(use-package all-the-icons
  :init
  ;; first time setup only
  ;; (all-the-icons-install-fonts)
  )
(use-package nerd-icons
  :init
  ;; first time setup only
  ;; (nerd-icons-install-fonts)
  )
(use-package fontawesome)
(use-package unicode-fonts)

(use-package consult
  :straight (consult
             :files ("*.el"))
  ;; :init
  ;; (advice-add #'register-preview :override #'consult-register-window)
  ;; (setopt xref-show-xrefs-function #'consult-xref
  ;;       xref-show-definitions-function #'consult-xref)
  :config
  (setopt consult-ripgrep-args (concat consult-ripgrep-args " -L"))
  ;;  (autoload 'projectile-project-root "projectile")
  ;; (setopt consult-project-root-function #'projectile-project-root)
  (defalias #'consult-imenu-variables #'consult-imenu)
  (defalias #'consult-imenu-functions #'consult-imenu)
  (defalias #'consult-imenu-commands #'consult-imenu)
  (defalias #'consult-imenu-macros #'consult-imenu)
  (defalias #'consult-imenu-packages #'consult-imenu)
  (defalias #'consult-imenu-types #'consult-imenu)
  ;; Configure initial narrowing per command
  (setopt consult-imenu-config
          '((emacs-lisp-mode :toplevel "Functions"
                             :types ((?f "Functions" font-lock-function-name-face)
                                     (?c "Commands"    font-lock-function-name-face)
                                     (?m "Macros"    font-lock-function-name-face)
                                     (?p "Packages"  font-lock-constant-face)
                                     (?t "Types"     font-lock-type-face)
                                     (?v "Variables" font-lock-variable-name-face))))
          )
  (setopt consult-initial-narrow-config
          '((consult-imenu-functions . ?f)
            (consult-imenu-commands . ?c)
            (consult-imenu-variables . ?v)
            (consult-imenu-packages . ?p)
            (consult-imenu-types . ?t)
            (consult-imenu-macros . ?m)))

  ;; Add initial narrowing hook
  (defun consult-initial-narrow ()
    (when-let (key (alist-get this-command consult-initial-narrow-config))
      (setopt unread-command-events (append unread-command-events (list key 32)))))
  (add-hook 'minibuffer-setup-hook #'consult-initial-narrow)

  (setopt xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

  ;; use vertico for minibuffer completion
  (setopt completion-in-region-function
          (lambda (&rest args)
            (apply (if vertico-mode
                       #'consult-completion-in-region
                     #'completion--in-region)
                   args)))


  (defvar consult--source-perspective-buffer
    `( :name     "Project Buffer"
       :narrow   ?b
       :category buffer
       :face     consult-buffer
       :history  buffer-name-history
       :state    ,#'consult--buffer-state
       :enabled  ,(lambda () consult-project-function)
       :items
       ,(lambda ()
          (when-let (root (consult--project-root))
            (consult--buffer-query :sort 'visibility
                                   :directory root
                                   :as #'buffer-name))))
    "Project buffer candidate source for `consult-buffer'.")
  )
(use-package consult-dir)
(use-package consult-notes
  :config
  (setopt consult-notes-file-dir-sources
          `(
            ("big-bend" ?b "~/workspaces/big-bend/notes")
            ("etc" ?e "~/workspaces/etc/notes")
            ("overdr0ne" ?o "~/.emacs.d/overdr0ne/notes")
            ("notes" ?n "~/notes")
            )))

(use-package marginalia)
(use-package ctrlf)

(use-package anzu
  :straight (anzu :type git
                  :host github
                  :repo "Overdr0ne/anzu")
  :config
  (cl-defun anzu--query-replace-common (use-regexp
                                        &key at-cursor thing prefix-arg (query t) isearch-p)
    (anzu--cons-mode-line 'replace-query)
    (when (and (use-region-p) (region-noncontiguous-p))
      (setopt anzu--region-noncontiguous (funcall region-extract-function 'bounds)))
    (let* ((use-region (use-region-p))
           (orig-point (point))
           (backward (anzu--replace-backward-p prefix-arg))
           (overlay-limit (anzu--overlay-limit backward))
           (beg (if (region-active-p)
                    (anzu--region-begin use-region (anzu--begin-thing at-cursor thing) backward)
                  (buffer-end -1)))
           (end (if (region-active-p)
                    (anzu--region-end use-region thing backward)
                  (buffer-end 1))
                )
           (prompt (anzu--query-prompt use-region use-regexp at-cursor isearch-p))
           (delimited (and current-prefix-arg (not (eq current-prefix-arg '-))))
           (curbuf (current-buffer))
           (clear-overlay nil))
      (when (and anzu-deactivate-region use-region)
        (deactivate-mark t))
      (unwind-protect
          (let* ((from (cond ((and at-cursor beg)
                              (setopt delimited nil)
                              (anzu--query-from-at-cursor curbuf beg end overlay-limit))
                             (isearch-p
                              (anzu--query-from-isearch-string
                               curbuf beg end use-regexp overlay-limit))
                             (t (anzu--query-from-string
                                 prompt beg end use-regexp overlay-limit))))
                 (to (cond ((consp from)
                            (prog1 (cdr from)
                              (setopt from (car from)
                                      anzu--total-matched anzu--last-replaced-count)))
                           ((string-match "\0" from)
                            (let ((replaced (substring-no-properties from (match-end 0))))
                              (setopt from (substring-no-properties from 0 (match-beginning 0)))
                              (if use-regexp
                                  (anzu--compile-replace-text replaced)
                                replaced)))
                           (t
                            (anzu--query-replace-read-to
                             from prompt beg end use-regexp overlay-limit)))))
            (anzu--clear-overlays curbuf (min beg end) (max beg end))
            (anzu--set-replaced-markers from beg end use-regexp)
            (setopt anzu--state 'replace anzu--current-position 0
                    anzu--replaced-markers (reverse anzu--replaced-markers)
                    clear-overlay t)
            (let ((case-fold-search (and case-fold-search (not at-cursor))))
              (if use-regexp
                  (apply #'perform-replace (anzu--construct-perform-replace-arguments
                                            from to delimited beg end backward query))
                (apply #'query-replace (anzu--construct-query-replace-arguments
                                        from to delimited beg end backward)))))
        (progn
          (unless clear-overlay
            (anzu--clear-overlays curbuf (min beg end) (max beg end)))
          (when (zerop anzu--current-position)
            (goto-char orig-point))
          (anzu--cleanup-markers)
          (anzu--reset-mode-line)
          (force-mode-line-update))))))
;; (use-package iedit)
(use-package nerd-icons)
(use-package doom-modeline
  :init
  (setopt doom-modeline-height 1)
  (setopt doom-modeline-minor-modes t))

(use-package expand-region)

;; (use-package kbd-mode)

(use-package flycheck
  ;;  :after (dash pkg-info let-alist seq)
  :config
  (setopt flycheck-emacs-lisp-load-path 'inherit)
  (setopt flycheck-indication-mode 'right-fringe)
  (setopt flycheck-check-syntax-automatically '(idle-change new-line mode-enabled))
  (add-hook 'flycheck-error-list-mode-hook visual-line-mode)
  )

(use-package diff-hl)
(use-package git-modes)

(use-package magit
  ;; :straight (magit :build (:not compile))
  :after transient
  :config
  (add-to-list 'exec-path "/usr/lib/git-core")
  (transient-replace-suffix 'magit-commit 'magit-commit-autofixup
    '("x" "Absorb changes" magit-commit-absorb))
  (transient-append-suffix 'magit-merge "-s" '("-a" "Allow unrelated histories" "--allow-unrelated-histories"))
  (setopt magit-buffer-log-args '("-n256" "--color" "--decorate" "--graph"))
  (put 'magit-log-mode 'magit-log-default-arguments
       magit-buffer-log-args)
  (defun dm/change-commit-author (arg)
    "Change the commit author during an interactive rebase in Magit.
With a prefix argument, insert a new change commit author command
even when there is already another rebase command on the current
line.  With empty input, remove the change commit author action
on the current line, if any."
    (interactive "P")
    (let ((author
           (magit-transient-read-person "Select a new author for this commit"
                                        nil
                                        nil)))
      (git-rebase-set-noncommit-action
       "exec"
       (lambda (_) (if author
                       (format "git commit --amend --author='%s'" author)
                     ""))
       arg)))

  ;;(define-key git-rebase-mode-map (kbd "h") #'dm/change-commit-author)
  )

(use-package format-all)

(use-package dts-mode
  :config
  (add-to-list 'auto-mode-alist '("defconfig\\'" . dts-mode))
  (add-to-list 'auto-mode-alist '("defconfig\\'" . dts-mode))
  (setopt auto-mode-alist (append auto-mode-alist '(("defconfig\\'" . dts-mode)
                                                  ("\\.its" . dts-mode)))))
;; (use-package devicetree-ts-mode
;;   )

(use-package kconfig-mode)
(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; langs
(use-package adaptive-wrap)

;; (use-package web-mode)
(use-package rainbow-mode)
;; (use-package sass-mode)
;; (use-package slim-mode)
;; (use-package nix-mode)
;; (use-package racket-mode)

(use-package rainbow-delimiters)

(use-package cmake-mode)

(use-package csv-mode)

;; (use-package py-isort)
;; (use-package pyimport)

(use-package bash-completion
  :config
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete))

;; (use-package request)

(use-package treemacs)
;; (use-package treemacs-perspective)

(use-package vi-tilde-fringe)

;;(use-package multiple-cursors)
;;
;;(use-package w3m)

;; (use-package find-file-in-project)

;; (use-package bookmark+)

(use-package system-packages
  :config
  (add-to-list 'system-packages-supported-package-managers
               '(pacaur .
                        ((default-sudo . nil)
                         (install . "pacaur -S")
                         (search . "pacaur -Ss")
                         (uninstall . "pacaur -Rs")
                         (update . "pacaur -Syu")
                         (clean-cache . "pacaur -Sc")
                         (log . "cat /var/log/pacman.log")
                         (get-info . "pacaur -Qi")
                         (get-info-remote . "pacaur -Si")
                         (list-files-provided-by . "pacaur -Ql")
                         (verify-all-packages . "pacaur -Qkk")
                         (verify-all-dependencies . "pacaur -Dk")
                         (remove-orphaned . "pacaur -Rns $(pacman -Qtdq)")
                         (list-installed-packages . "pacaur -Qe")
                         (list-installed-packages-all . "pacaur -Q")
                         (list-dependencies-of . "pacaur -Qi")
                         (noconfirm . "--noconfirm"))))
  (setopt system-packages-use-sudo t)
  (setopt system-packages-package-manager 'aptitude))

;; (use-package web-search)

(use-package paredit)
;; (use-package macrostep)
(use-package smartparens)
(use-package smart-tabs-mode)

;; (use-package lispy)

(use-package clojure-mode)
(use-package cider)
;; (use-package elein)

(use-package ein)

(use-package notifications)

(use-package systemd)

(use-package dired
  :straight (:type built-in)
  :config
  (setopt dired-listing-switches "-alhF"))
(use-package diredfl)
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (setopt all-the-icons-dired-monochrome nil))
(use-package dired-narrow)
;;(use-package dired-du)
;;(use-package dired-subtree
;;  :config
;;  (setopt dired-subtree-use-backgrounds nil))
;;(use-package dired-filter)

(use-package highlight)
(use-package highlight-defined)
(use-package hl-line
  :straight (hl-line :type built-in)
  :config
  (defun enable-hl-line-mode ()
    (interactive)
    (hl-line-mode +1)))

(use-package flyspell
  :straight (flyspell :type built-in)
  :config
  ;; flyspell should not be active until correcting
  )

;; (use-package rcirc
;;   :config
;;   (rcirc-track-minor-mode t)
;;   (add-hook 'rcirc-mode-hook (lambda ()
;;                                (flyspell-mode 1)
;;                                (rcirc-omit-mode)))

;;   (setopt rcirc-server-alist
;;         '(("irc.libera.chat" :channels ("#emacs"))))
;;   (evil-set-initial-state 'rcirc-mode 'insert)
;;   (setopt rcirc-buffer-maximum-lines 1000)
;;   (setopt rcirc-default-nick "Overdr0ne")
;;   (setopt rcirc-default-user-name "Overdr0ne")
;;   (setopt rcirc-log-flag t))

;; (use-package explain-pause-mode)

(use-package daemons
  :config
  (setopt daemons-systemd-color t))

;; (use-package pretty-mode
;;   :config
;;   (global-pretty-mode t)
;;   (pretty-deactivate-groups
;;    '(:equality :ordering :ordering-double :ordering-triple
;;                 :arrows :arrows-twoheaded :punctuation
;;                 :logic :sets))
;;   (pretty-activate-groups
;;    '(:sub-and-superscripts :greek :arithmetic-nary)))

;; (use-package wordnut)

;; (use-package xwwp)

;; (use-package calfw)

(use-package minions
  :custom
  (minions-mode-line-lighter "⚙")
  (minions-mode-line-delimiters nil)
  (minions-direct '(overwrite-mode parinfer-mode))
  )

(use-package which-key)

;; (use-package sfs
;;   ;; :straight (sfs :type git
;;   ;;                :host github
;;   ;;                :repo "Overdr0ne/sfs"
;;   ;;                :branch "master"
;;   :straight (sfs :local-repo "~/src/sfs"
;;                  :files ("sfs.el"
;;                          "sfs-recoll.el"
;;                          "sfs-tui.el"
;;                          "sfs-tag.el"
;;                          "sfs-reindex.el"
;;                          "service.py"))
;;   :config
;;   ;; (add-to-list 'evil-insert-state-modes 'sfs-research-mode)
;;   (global-sfs-mode 1))

;; themes
;; (use-package load-theme-buffer-local)
;; (use-package nofrils-acme-theme)
;; (use-package plan9-theme)
;; (use-package acme-theme
;;   :config
;;   (load-theme 'acme t))
(use-package gruvbox-theme)
(use-package alect-themes)
;; (use-package adwaita
;;   :config
;;   (load-theme 'adwaita 't))
;; (use-package dichromacy
;;   :config
;;   (load-theme 'dichromacy 't))

(use-package dracula-theme
  :straight (dracula-theme :type git
                           :host github
                           :repo "Overdr0ne/emacs"))

(use-package imenu
  :config
  (setopt imenu-generic-expression '(("Section"
                                      "^;;;"))))

(use-package disk-usage)

;; (use-package good-scroll)
(use-package ultra-scroll
  :straight (ultra-scroll :type git
                          :host github
                          :repo "jdtsmith/ultra-scroll"
                          :branch "main"
                          :files ("*.el")))

(use-package perspective
  :config
  (setopt persp-suppress-no-prefix-key-warning t)
  (setopt persp-show-modestring nil)
  ;; (add-hook 'persp-mode-hook (lambda () (setopt read-buffer-function nil)))

  ;; (add-hook 'persp-mode-hook
  ;;           (lambda ()
  ;;             (setopt gumshoe-slot-schema '(time perspective buffer position line))
  ;;             (setopt gumshoe-footprint-strategy 'delete-overlapping)
  ;;             ;; (setopt gumshoe-footprint-strategy 'nil)
  ;;             (setopt gumshoe-backlog-type 'ring)
  ;;             ;; (setopt gumshoe-backlog-type 'tree)
  ;;             (add-to-list 'load-path "~/src/gumshoe")
  ;;             (load "~/src/gumshoe/gumshoe.el")
  ;;             ))
  )

(use-package gumshoe
  ;; :straight (gumshoe :type git
  ;;                    :host github
  ;;                    :repo "Overdr0ne/gumshoe"
  ;;                    :branch "master"
  ;;                    ;; :branch "feature/footprint-strategy"
  ;;                    )
  :straight (gumshoe :local-repo "~/src/gumshoe")
  :init
  ;; (setopt gumshoe-slot-schema '(time perspective buffer position line))
  ;; (setopt gumshoe-footprint-strategy 'delete-overlapping)
  (setopt gumshoe-slot-schema '(time buffer position line))
  (setopt gumshoe-backlog-type 'ring)
  
  ;;  (global-gumshoe-persp-mode 1)
  ;; (setopt gumshoe-slot-schema '(time perspective buffer position line))
  ;; (advice-add #'gumshoe-peruse-globally :around
  ;;              (lambda (old-fn)
  ;;                (let ((selectrum-should-sort nil))
  ;;                  (funcall old-fn))))
  ;; (advice-add #'gumshoe-peruse-in-persp :around
  ;;              (lambda (old-fn)
  ;;                (let ((selectrum-should-sort nil))
  ;;                  (funcall old-fn))))
  ;; (advice-add #'gumshoe-peruse-in-buffer :around
  ;;              (lambda (old-fn)
  ;;                (let ((selectrum-should-sort nil))
  ;;                  (funcall old-fn))))
  )

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "HISTFILE")
  (defun turn-on-comint-history (history-file)
    (setopt comint-input-ring-file-name history-file)
    (comint-read-input-ring 'silent))
  ;; (setopt shell-command-history '())

  (add-hook 'shell-mode-hook
            (lambda ()
              (turn-on-comint-history (getenv "HISTFILE"))))
  (add-hook 'kill-buffer-hook #'comint-write-input-ring)
  (add-hook 'kill-emacs-hook
            (lambda ()
              (--each (buffer-list)
                (with-current-buffer it (comint-write-input-ring))))))

(use-package shelldon
  ;;   :straight (shelldon :type git
  ;;                       :host github
  ;;                       :repo "Overdr0ne/shelldon"
  ;;                       :branch "master"
  ;;                       :files ("shelldon.el"))
  :straight (shelldon :local-repo "~/src/shelldon")
  :init
  (start-file-process-shell-command "ls" "test" "ls -l ~")
  ;; tell bash this shell is interactive
  (setopt shell-command-switch "-ic")
  ;; (setopt shell-command-switch "-c")
  (setopt shelldon-autohistory-p t)
  (setopt shelldon-desktop-notify-p t)
  ;; recursive minibuffers for nested autocompletion from minibuffer commands,
  ;; to e.g. interactively select from the kill-ring
  (setopt enable-recursive-minibuffers t)
  ;; autohistory automatically shows your command history as you type
  ;; using your default autocompletion engine
  (setopt shelldon-autohistory-p t)
  (setopt global-shelldon-async-p nil)
  ;; comint output may contain SGR control sequences that may be translated into
  ;; text properties if emacs has something equivalent. This requires special
  ;; processing.
  (add-hook 'shelldon-mode-hook 'ansi-color-for-comint-mode-on)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  ;; (add-to-list 'display-buffer-alist
  ;;              '("*shelldon:"
  ;;                (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window display-buffer-pop-up-window)
  ;;                (side . bottom)
  ;;                (slot . 0)
  ;;                (window-height . (lambda (win) (sit-for 1) (fit-window-to-buffer win 20)))
  ;;                ))
  (defun shelldon-fit-window-to-buffer (&optional window max-height min-height max-width min-width preserve-size)
    (set-window-point win 0)
    (fit-window-to-buffer win 20))

  (add-to-list 'display-buffer-alist
               '("*shelldon:"
                 (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window display-buffer-pop-up-window)
                 (side . bottom)
                 (slot . 0)
                 (window-width . 80)
                 (window-height . fit-window-to-buffer))))

;; completion at point overlay
(use-package corfu
  :init
  (defun corfu-setup-advice ()
    (defvar corfu-mode-map-alist)
    (setopt corfu-mode-map-alist `((completion-in-region-mode . ,corfu-map)))
    (add-to-list 'emulation-mode-map-alists 'corfu-mode-map-alist))
  (setopt corfu-quit-at-boundary nil)
  ;; (defun corfu-enable-in-minibuffer ()
  ;;   "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  ;;   (when (where-is-internal #'completion-at-point (list (current-local-map)))
  ;;     ;; (setopt-local corfu-auto nil) ;; Enable/disable auto completion
  ;;     (setopt-local corfu-echo-delay nil ;; Disable automatic echo and popup
  ;;                 corfu-popupinfo-delay nil)
  ;;     (corfu-mode 1)))
  ;; (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  ;; (advice-add 'corfu--setup :before #'corfu-setup-advice)
  ;; :config
  ;; (add-hook 'prog-mode-hook 'corfu-mode)
  )

;; icons for corfu
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  ;;  :after (corfu)
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-line)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  :config
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package dtrt-indent)
(use-package highlight-indentation
  :straight (highlight-indentation :type git
                                   :host github
                                   :repo "antonj/Highlight-Indentation-for-Emacs"
                                   :branch "master"))

;; great package but terrible performance
;; very noticable when scrolling
;; (use-package indent-guide
;;   :straight (indent-guide :type git
;;                           :host github
;;                           :repo "zk-phi/indent-guide"
;;                           :branch "master"))

(use-package buffer-move)

;; (use-package mastodon)

;; (use-package sclang
;;   :straight (sclang :type git
;;                     :host github
;;                     :repo "Overdr0ne/scel"
;;                     :branch "main"))
;; (use-package sclang)
;; (setopt sclang-help-path '("/home/sam/.local/share/SuperCollider/Help"))
;; (use-package sclang-extensions)
;; (use-package sclang-snippets)

;; (use-package haskell-mode)

(use-package treepy)

(use-package kactivities
  :straight (kactivities :local-repo "~/src/emacs-kde")
  )

(use-package firmware
  :straight (firmware :local-repo "~/src/firmware-el")
  :config
  (setopt etc-power-golden (firmware-target :name "golden" :power-ip "10.102.3.11" :power-port "Outlet1"))
  (setopt etc-power-fdrive (firmware-target :name "fdrive" :power-ip "10.102.3.11" :power-port "Outlet2"))
  (setopt firmware-targets '(etc-power-golden etc-power-fdrive))
  )

(use-package completionist
  :straight (completionist :local-repo "~/src/completionist"
                           :files ("*.el" "extensions/*.el"))
  :after (perspective)
  ;; (when t
  ;;   (add-to-list 'load-path "~/src/completionist")
  ;;   (load "~/src/completionist/completionist.el")
  ;;   (load "~/src/completionist/extensions/completionist-flat.el")
  ;;   (load "~/src/completionist/extensions/completionist-mouse.el")
  :init
  (defun completionist-persp-switch ()
    (interactive)
    (let ((window-sides-slots '(2 2 2 2))
          (action '((display-buffer-in-side-window)
                    (window-height . 1)
                    (preserve-size . t)
                    (side . top)
                    (slot . 0))))
      (completionist--complete "persp:" #'persp-names #'persp-switch " *persps*" action)
      ;; (completionist--exhibit (get-buffer " *persps*"))
      ))
  (defun completionist-persp-switch-unfocused ()
    (interactive)
    (unless (minibufferp (current-buffer))
      (let ((window-sides-slots '(2 2 2 2))
            (action '((display-buffer-in-side-window)
                      (window-height . 1)
                      (preserve-size . t)
                      (side . top)
                      (slot . 0))))
        (completionist--complete "persp:" #'persp-names #'persp-switch " *persps*" action t)
        (completionist--exhibit (get-buffer " *persps*")))))
  (defun process-names ()
    (mapcar #'process-name
            (process-list)))
  (defun completionist-kill-process ()
    (interactive)
    ;; (mapcar (lambda (process)
    ;;           (concat (process-name process) "|" (symbol-name (process-type process)) "|" (symbol-name (process-status process))))
    ;;         (process-list))
    (completionist-persp-switch-unfocused #'process-names
                                          (lambda (process-name) (signal-process process-name 'SIGABRT)))
    )
  (defun completionist-process-buffer-switch ()
    (interactive)
    (let ((window-sides-slots '(3 3 3 3))
          (action '((display-buffer-in-side-window)
                    (window-height . 1)
                    (preserve-size . t)
                    (side . top)
                    (slot . 2))))
      (unwind-protect
          (completionist--complete "processes:" #'process-names
                                   (lambda (process) (switch-to-buffer (process-buffer process)))
                                   "*comp-procs*"
                                   action)
        )))
  (add-hook 'persp-created-hook #'completionist-persp-switch-unfocused)
  ;; (remove-hook 'persp-created-hook #'completionist-persp-switch-unfocused)
  )

(use-package embark-consult
  ;; :hook
  ;; (embark-collect-mode . consult-preview-at-point-mode)
  )

(use-package embark
  :config
  (setopt embark-cycle-key "<return>")
  ;; (setopt embark-prompter 'embark-completing-read-prompter)
  (setopt embark-prompter 'embark-keymap-prompter)
  ;; (setopt embark-verbose-indicator-display-action
  ;;         '((display-buffer-in-side-window)
  ;;           (side . bottom)))
  ;; (setopt embark-verbose-indicator-display-action
  ;;         '(
  ;;           (display-buffer-reuse-window)
  ;;           ))
  (setopt embark-verbose-indicator-display-action
          '(
            (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window display-buffer-pop-up-window)
            (side . right)
            (slot . 0)
            (window-width . 80)
            ))
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  ;; (setopt embark-action-indicator
  ;;       (lambda (map _target)
  ;;         (which-key--show-keymap "Embark" map nil nil 'no-paging)
  ;;         #'which-key--hide-popup-ignore-command)
  ;;       embark-become-indicator embark-action-indicator)
  )
;; (use-package esup)

;; (use-package package-lint)

;; (use-package bufler)

;; (use-package burly)

;; (use-package prism
;;   :straight (prism :type git
;;                    :host github
;;                    :branch "master"
;;                    :repo "Overdr0ne/prism.el")
;;   :config
;;   (setopt prism-num-faces 30
;;          prism-color-attribute :background
;;          prism-colors (list "maroon" "violet" "blue violet" "blue" "light sea green" "green" "yellow green" "yellow" "orange" "red")
;;          prism-desaturations (list 0 0 0)
;;          prism-lightens (list 0 0 0)
;;          prism-opacities (list 10 30 90)
;;          prism-comments-fn (lambda (color) color)
;;          prism-strings-fn (lambda (color) color)
;;          prism-parens-fn (lambda (color) color))
;;   ;; (prism-save-colors)
;;  )

;; (load "~/src/winblows/winblows.el")

;; (use-package loccur)

(use-package bitbake
  :straight (bitbake :type git
                     :host github
                     :repo "Overdr0ne/bitbake-el"
                     :branch "master"
                     :build (:not compile))
  :config
  (defun sam-bitbake ()
    "Read command with bitbake as prefix."
    (interactive)
    (let ((cmd (intern-soft (sam-read-extended-command "bitbake- "))))
      (command-execute cmd 'record)))

  (setopt bitbake-flash-device "/dev/mmcblk0")
  ;; (setopt bitbake-poky-directory "/home/sam/workspaces/dtech/layers")
  ;; (setopt bitbake-build-directory "/home/sam/workspaces/dtech/build")
  ;; (setopt bitbake-poky-directory "/home/sam/workspaces/impinj")
  ;; (setopt bitbake-build-directory "/home/sam/workspaces/impinj/build")
  (setopt bitbake-poky-directory "/home/sam/workspaces/impinj/container/build")
  (setopt bitbake-build-directory "/home/sam/workspaces/impinj/container/build/build")
  ;; (setopt bitbake-poky-directory "/home/sam/tmp/impinj")
  ;; (setopt bitbake-build-directory "/home/sam/tmp/impinj/build")
  ;; (setopt bitbake-poky-directory "/home/sam/workspaces/atlas/build/")
  ;; (setopt bitbake-build-directory "/home/sam/workspaces/atlas/build")
  )

(use-package docker
  :config
  (setopt docker-command "podman")
  ;; (setopt docker-run-as-root t)
  (setopt docker-image-run-default-args '("-i" "-t" "--rm")))

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; (use-package keychain-environment
;;   :straight (keychain-environment :type git
;;                    :host github
;;                    :repo "tarsius/keychain-environment"
;;                    :branch "master"))
;; (use-package wrap-region-mode
;;   :straight (wrap-region-mode :type git
;;                    :host github
;;                    :repo "rejeep/wrap-region.el"
;;                    :branch "master")
;;   )

(use-package ediff
  :straight (:type built-in)
  :init
  (setopt ediff-window-setup-function #'ediff-setup-windows-plain)
  (add-hook 'ediff-mode-hook #'ediff-reload-keymap)
  (defvar my-ediff-last-windows nil)

  (defun my-store-pre-ediff-winconfig ()
    (setopt my-ediff-last-windows (current-window-configuration)))

  (defun my-restore-pre-ediff-winconfig ()
    (set-window-configuration my-ediff-last-windows))

  (add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
  (add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig)
  )

;; (use-package elgrep)

;; (use-package rg)

(use-package slink
  :straight (slink :type git
                   :host github
                   :repo "Overdr0ne/slink"
                   :branch "main"))

(use-package geiser)
(use-package geiser-guile
  )
(use-package bui)
(use-package edit-indirect)
(use-package build-farm)
(use-package magit-popup)
(use-package guix
  :config
  (setopt guix-home-profile "/home/sam/.guix-profile"))

(use-package recentf
  :straight (recentf :type built-in)
  :init
  (setopt recentf-max-menu-items 500)
  (setopt recentf-max-menu-items 600)
  (defconst recentf-used-hooks
    '(
      (find-file-hook       recentf-track-opened-file)
      (dired-mode-hook       recentf-track-opened-file)
      (write-file-functions recentf-track-opened-file)
      (kill-buffer-hook     recentf-track-closed-file)
      (kill-emacs-hook      recentf-save-list)
      )
    "Hooks used by recentf.")

  (defun recentf-track-opened-file ()
    "Insert the name of the file just opened or written into the recent list."
    (if buffer-file-name
        (progn (recentf-push buffer-file-name))
      (when default-directory
        (recentf-push default-directory)))
    ;; Must return nil because it is run from `write-file-functions'.
    nil)
  )

;; (use-package tempel
;;   :config
;;   ;; (add-to-list 'completion-at-point-functions #'tempel-expand)
;;   (setopt tempel-path "~/.emacs.d/overdr0ne/template.el")
;;   )

(use-package yasnippet
  :config
  (define-minor-mode sam/yas-insert-mode
    "Transient mode for inserting yasnippets."
    :keymap (let ((map (make-sparse-keymap)))
              map))

  (defun sam/yas-insert-snippet ()
    (interactive)
    (let* ((s)
           (mm major-mode)
           (action '((display-buffer-in-side-window)
                     (preserve-size . t)
                     (side . bottom)
                     (slot . 0))))
      (with-current-buffer (pop-to-buffer "yasnippet-term" action)
        (funcall mm)
        (sam/yas-insert-mode +1)
        (call-interactively 'consult-yasnippet)
        (recursive-edit)
        (setopt s (buffer-string)))
      (term-send-raw-string s)
      (kill-buffer "yasnippet-term")))

  ;; TODO: define snippet creator

  (skey-define-keys
   '(sam/yas-insert-mode-map)
   `(
     ("<tab>" yas-next-field)
     ("<backtab>" yas-prev-field)
     ("<return>" exit-recursive-edit)
     ))

  (add-to-list 'yas-snippet-dirs "~/src/guix/etc/snippets/yas"))
(use-package yasnippet-snippets)
(use-package consult-yasnippet)

(use-package cmake-mode)

(use-package yaml-mode)

(use-package meson-mode)

;; (use-package mode-minder
;;   :straight (mode-minder :type git
;;                          :host github
;;                          :repo "jdtsmith/mode-minder"))

(use-package avy)

;; (eval-after-load 'avy
;;   (progn
;;     (load "~/src/holymotion/holymotion.el")
;;     ;;(holymotion-make-motion
;;     ;; holymotion-backward-whitespace #'sp-backward-whitespace
;;     ;; :scope 'line))
;;     ))
(use-package holymotion
  :straight (holymotion :local-repo "~/src/holymotion")
  ;; :straight (holymotion :type git
  ;;                       :host github
  ;;                       :repo "Overdr0ne/holymotion"
  ;;                       :branch "main"
  ;;                       ;; :files ("*.el")
  ;;                       )
  :after (avy)
  :config
  (holymotion-make-motion
   holymotion-forward-beginning-of-defun #'forward-beginning-of-defun)
  (holymotion-make-motion
   holymotion-forward-sexp #'backward-up-list)
  (holymotion-make-motion
   holymotion-forward-sexp #'evim-forward-to-sexp)
  (holymotion-make-motion
   holymotion-backward-sexp #'backward-sexp))
(use-package all-the-icons-completion
  :after (all-the-icons)
  :init
  (setopt all-the-icons-scale-factor 0.9)
  (all-the-icons-completion-mode +1))

(use-package evim
  :straight (evim
             :local-repo "~/src/evim"))
;; (use-package wrap-region
;;   :config
;;   (wrap-region-add-wrapper "~" "~" "~" '(org-mode))
;;   ;; (wrap-region-global-mode +1)
;;   )

(use-package ggtags)

(use-package eglot
  ;; :config
  ;; (setopt eglot-withhold-process-id "1")
  ;; (with-eval-after-load 'eglot
  ;; (add-to-list 'eglot-server-programs `(c-mode .     ("~sam/workspaces/legend/hepafilter700/Docker/" "metio/devcontainers-nodejs" "c-language-server --stdio"))))
  :straight (eglot :type built-in)
  :config
  )

(use-package lsp-scheme)

;; (use-package scel)

(use-package extempore-mode)

(use-package markdown-toc)

(use-package jenkinsfile-mode)

(use-package vundo)

(use-package swift-mode)

(use-package remember
  :straight (remember :type built-in)
  :config
  (defun sam-format-remember-text (text)
    (concat remember-leader-text
            (format-time-string remember-time-format)
            "\n" text))
  (setopt remember-data-file "~/notes/mem.md")
  (setopt remember-data-directory "~/notes/")
  (setopt remember-notes-initial-major-mode 'text-mode)
  ;; (skey-define-keys
  ;;  '(remember-mode-map)
  ;;  `(("C-g" remember-destroy)))
  )

(use-package kotlin-mode)
;; (use-package kotlin-ts-mode)

(use-package hierarchy
  :straight (hierarchy :type built-in))

(use-package org
  :straight (org :type built-in)
  :config
  (setopt org-default-notes-file "~/notes/global.org")
  )

(use-package org-agenda
  :straight (org-agenda :type built-in)
  :config
  (setopt org-agenda-files '("~/notes/")))

(use-package org-agenda
  :straight (org-agenda :type built-in)
  :config
  (setopt org-agenda-files '("~/notes/"))
  )

(use-package org-capture
  :straight (org-capture :type built-in)
  :config
  (add-to-list 'org-capture-templates
               '("t" "todo" entry (file "~/notes/todo.org")
	               "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t))
  (add-to-list 'org-capture-templates
               '("d" "Diary" entry (file+datetree "~/notes/diary.org")
	               "* %?\n%U\n" :clock-in t :clock-resume t))
  (add-to-list 'org-capture-templates
               '("i" "Idea" entry (file "~/notes/ideas.org")
	               "* %?\n%t"))
  )

;; (use-package deft)

;; (use-package helm)

(use-package neotree
  :config
  (setopt neo-smart-open t)
  (setopt neo-theme 'icons))

(use-package aptitude
  :straight (aptitiude
             :local-repo "~/src/aptitude"))

(use-package linux-commands
  :straight (linux-commands
             :local-repo "~/src/linux-commands-el"
             :type nil))

(use-package itail)

(use-package clean-kill-ring)

(use-package launch)

(use-package string-inflection)

(use-package markdown-mode
  :straight (markdown-mode
             :local-repo "~/src/markdown-mode"))

(use-package gptel
  :straight (gptel
             :files ("*.el"))
  :config
  (require 'gptel-integrations)
  ;; (setopt gptel-gpt-backend
  ;;       (gptel-make-anthropic "GPT"
  ;;         :stream t
  ;;         :key (f-read-text (expand-file-name (concat overdr0ne-directory "/keys/chatgpt.key")))
  ;;         ))
  ;; (setopt gptel-openai-backend
  ;;       (gptel-make-openai
  ;;           "ChatGPT"
  ;;         :key (f-read-text (concat overdr0ne-directory "/keys/chatgpt.key"))
  ;;         :stream t
  ;;         :models '("gpt-3.5-turbo" "gpt-3.5-turbo-16k" "gpt-4o-mini"
  ;;                   "gpt-4" "gpt-4o" "gpt-4-turbo" "gpt-4-turbo-preview"
  ;;                   "gpt-4-32k" "gpt-4-1106-preview" "gpt-4-0125-preview")))
  (setopt gptel-display-buffer-action
          '((display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window display-buffer-pop-up-window)
            (side . right)
            (slot . 0)
            (window-width . 80)))

  (setopt gptel-claude-backend
          (gptel-make-anthropic "Claude"
            :stream t
            :key (f-read-text (expand-file-name (concat overdr0ne-directory "/keys/anthropic.key")))))

  (setopt gptel-backend gptel-claude-backend)
  (setopt gptel-model 'claude-3-7-sonnet-20250219)
  ;;   (setopt gptel-model "gpt-4o-mini")
  ;; (setopt gptel-backend gptel--openai)

  (gptel-make-tool
   :name "read_buffer"
   :function (lambda (buffer)
               (unless (buffer-live-p (get-buffer buffer))
                 (error "error: buffer %s is not live." buffer))
               (with-current-buffer  buffer
                 (buffer-substring-no-properties (point-min) (point-max))))
   :description "return the contents of an emacs buffer"
   :args (list '(:name "buffer"
                       :type string
                       :description "the name of the buffer whose contents are to be retrieved"))
   :category "emacs")

  (gptel-make-tool
   :name "create_file"
   :function (lambda (path filename content)
               (let ((full-path (expand-file-name filename path)))
                 (with-temp-buffer
                   (insert content)
                   (write-file full-path))
                 (format "Created file %s in %s" filename path)))
   :description "Create a new file with the specified content"
   :args (list '(:name "path"             ; a list of argument specifications
	                     :type string
	                     :description "The directory where to create the file")
               '(:name "filename"
	                     :type string
	                     :description "The name of the file to create")
               '(:name "content"
	                     :type string
	                     :description "The content to write to the file"))
   :category "filesystem")                ; An arbitrary label for grouping
  )

;; (when t
;;   (add-to-list 'load-path "~/src/repllm")
;;   (load "~/src/repllm/repllm.el")
;;   (load "~/src/repllm/repllm-curl.el")
;;   (load "~/src/repllm/repllm-openai.el")
;;   (setopt repllm-model "gpt-4o-mini")
;;   (setopt repllm-api-key (f-read-text (expand-file-name (concat overdr0ne-directory "/keys/chatgpt.key"))))
;;   (setopt repllm-openai-backend
;;           (repllm-make-openai "GPT"
;;             :stream t
;;             :key (f-read-text (expand-file-name (concat overdr0ne-directory "/keys/chatgpt.key")))
;;             ))
;;   )

(use-package aider
  :straight (:host github :repo "tninja/aider.el")
  :config
  ;; For latest claude sonnet model
  (setq aider-args '("--model" "sonnet" "--no-auto-accept-architect"))
  (setenv "ANTHROPIC_API_KEY" (f-read-text (expand-file-name (concat overdr0ne-directory "/keys/anthropic.key"))))
  ;; Or chatgpt model
  ;; (setq aider-args '("--model" "o4-mini"))
  ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
  ;; Or gemini model
  ;; (setq aider-args '("--model" "gemini-exp"))
  ;; (setenv "GEMINI_API_KEY" <your-gemini-api-key>)
  ;; Or use your personal config file
  ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
  ;; ;;
  ;; Optional: Set a key binding for the transient menu
  ;; (global-set-key (kbd "C-c a") 'aider-transient-menu)
  )

(use-package yequake
  :straight (yequake :fetcher github :repo "alphapapa/yequake")
  :config
  (setq yequake-frames
        '(
          ("terminal" .
           ((width . 0.90)
            (height . 0.5)
            (alpha . 0.95)
            ;; (buffer-fns . ((lambda () (interactive) (ansi-term "/usr/bin/zsh"))))
            (buffer-fns . (sam-switch-to-recent-term))
            ;; (buffer-fns . ((lambda () (interactive) (gptel-menu))))
            (frame-parameters . ((undecorated . t)))))
          ("gptel" .
           ((width . 0.90)
            (height . 0.5)
            (alpha . 0.95)
            ;; (buffer-fns . ((lambda () (interactive) (ansi-term "/usr/bin/zsh"))))
            ;; (buffer-fns . (sam-switch-to-recent-term))
            (buffer-fns . ((lambda () (interactive) (gptel "Claude"))))
            (frame-parameters . ((undecorated . t)))))
          ;; ("Yequake & scratch" .
          ;;  ((width . 0.65)
          ;;   (height . 0.4)
          ;;   (alpha . 0.95)
          ;;   (buffer-fns . ("~/"
          ;;                  split-window-horizontally
          ;;                  "*scratch*"))
          ;;   (frame-parameters . ((undecorated . t)))))
          )))

;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :ensure t
;;   :bind (
;;          :map copilot-completion-map
;;               ("C-c TAB" . 'copilot-accept-completion))
;;   :config
;;   (set-face-attribute 'copilot-overlay-face nil :foreground "grey30"))

(use-package minuet
  :after (evim)
  :bind
  (("C-M-l" . #'minuet-show-suggestion)
   :map minuet-active-mode-map
   ("C-M-l" . #'minuet-accept-suggestion)
   ("C-l" . #'minuet-accept-suggestion)
   ("C-p" . #'minuet-previous-suggestion)
   ("C-n" . #'minuet-next-suggestion)))

;; (use-package spookfox
;;   :straight
;;   (spookfox :type git
;;             :host github
;;             :repo "bitspook/spookfox"
;;             :files ("lisp/*.el" "lisp/apps/*.el"))
;;   :config
;;   (spookfox-init))

(use-package app-launcher
  :straight (app-launcher :local-repo "~/src/app-launcher")
  ;; :straight (app-launcher
  ;;            :type git
  ;;            :fetcher github
  ;;            :branch "main"
  ;;            :repo "SebastienWae/app-launcher")
  )

;; (use-package json-rpc)

(use-package mcp
  :straight (:host github :repo "lizqwerscott/mcp.el")
  :custom
  (mcp-hub-servers
   `(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/home/sam/src/")))
     ;; ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
     ))
  :hook (after-init . mcp-hub-start-all-server))

(provide '+modules)
;;; +modules.el ends here
