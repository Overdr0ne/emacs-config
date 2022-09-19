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

;; (require 'init)

(require 'use-package)
(require 'straight)
(require 'emacs)

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

(use-package text-property-search
  :straight (text-property-search :type built-in))

(use-package rx
  :straight (rx :type built-in))

(use-package wid-edit
  :straight (wid-edit :type built-in
                      ;;:build (:not compile)
                      )
  )

(use-package cl-lib
  :straight (cl-lib :type built-in))

(use-package compilation-mode
  :straight (compilation-mode :type built-in))

;; (use-package compat)

(use-package transient
;;  :after (compat)
  )

;; (use-package hydra)

;; (use-package no-littering)

;; (use-package general)
;;
;;;;(use-package tree-sitter-langs)
;;;;(use-package tree-sitter
;;;;  (require 'tree-sitter-langs)
;;;;  (global-tree-sitter-mode)
;;;;  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
;;
;;;;(use-package erc)
;;
;;(use-package sql)

(use-package posframe)

(use-package helpful)

(use-package winner
  :straight (winner :type built-in)
  :init
  (winner-mode +1)
  )

;; (use-package tramp
;;   :straight (tramp :build (:not compile
;;                                 :not native-compile)))

(use-package vertico
  :straight (vertico
	         :files ("*.el" "extensions/*.el"))
  :init
  (vertico-mode +1)
  ;; :config
  ;; Enable vertico-multiform
  ;; (vertico-multiform-mode +1)

  ;; Configure the display per command.
  ;; Use a buffer with indices for imenu
  ;; and a flat (Ido-like) menu for M-x.
  ;;  (setq vertico-multiform-commands
  ;;        '((gumshoe-peruse-globally (vertico-sort-function . nil))
  ;;          (gumshoe-peruse-in-buffer (vertico-sort-function . nil))
  ;;          (gumshoe-peruse-in-window (vertico-sort-function . nil))
  ;;          ;; (consult-imenu buffer indexed)
  ;;          ;; (persp-switch-to-buffer* (vertico-sort-function . vertico-sort-history-alpha))
  ;;          ))

  ;; (setq vertico-multiform-categories
  ;;       '(
  ;;         ;; (file grid)
  ;;         ;; (symbol (vertico-sort-function . vertico-sort-alpha))
  ;;         ;; (file (vertico-sort-function . sort-directories-first))
  ;;         (consult-grep buffer)))

  ;; Sort directories before files
  ;; (defun sort-directories-first (files)
  ;;   (setq files (vertico-sort-history-length-alpha files))
  ;;   (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
  ;;          (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))
  )
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
(use-package savehist
;;  :after (projectile)
  :init
  ;;  (require 'projectile)
  (setq history-length 500)
  (setq savehist-file "~/.emacs.d/history")
  (savehist-mode +1)
  ;; (add-to-list 'savehist-additional-variables
  ;;              'projectile-relevant-known-projects)
  (setq savehist-additional-variables
        (append savehist-additional-variables
                '(kill-ring search-ring regexp-search-ring compile-history log-edit-comment-ring obarray))))

;; (use-package projectile
;;   :straight (projectile
;;              ;; :build (:not compile)
;;              )
;;   :config
;;   (projectile-mode +1)
;;   (defun sam-projectile-vc-or-dired ()
;;     (interactive)
;;     (if (not (string-equal (projectile-project-vcs) "none"))
;;         (progn (projectile-vc)
;;                (call-interactively #'find-file))
;;       (dired default-directory)))
;;   (setq projectile-switch-project-action #'sam-projectile-vc-or-dired)

;;   (delete "~/.emacs.d/.local/" projectile-globally-ignored-directories)
;;   (add-to-list 'projectile-globally-ignored-directories "build")
;;   (defun sam-projectile-ibuffer-by-project (project-root)
;;     "Open an IBuffer window showing all buffers in PROJECT-ROOT."
;;     (let ((project-name (funcall projectile-project-name-function project-root)))
;;       (ibuffer t (format "*%s Buffers*" project-name)
;;                (list (cons 'projectile-files project-root)) nil t)))

;;   (defun sam-projectile-ibuffer (prompt-for-project)
;;     "Open an IBuffer window showing all buffers in the current project.

;; Let user choose another project when PROMPT-FOR-PROJECT is supplied."
;;     (interactive "P")
;;     (let ((project-root (if prompt-for-project
;;                             (projectile-completing-read
;;                              "Project name: "
;;                              (projectile-relevant-known-projects))
;;                           (projectile-project-root))))

;;       (sam-projectile-ibuffer-by-project project-root)))
;;   (add-hook 'projectile-after-switch-project-hook 'delete-other-windows))

(use-package consult
  :straight (consult
             ;; :build (:not compile)
             )
;;  :after (imenu)
  ;; :init
  ;; (advice-add #'register-preview :override #'consult-register-window)
  ;; (setq xref-show-xrefs-function #'consult-xref
  ;;       xref-show-definitions-function #'consult-xref)
  :config
;;  (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  (defalias #'consult-imenu-variables #'consult-imenu)
  (defalias #'consult-imenu-functions #'consult-imenu)
  (defalias #'consult-imenu-macros #'consult-imenu)
  (defalias #'consult-imenu-packages #'consult-imenu)
  (defalias #'consult-imenu-types #'consult-imenu)
  ;; Configure initial narrowing per command
  (defvar consult-initial-narrow-config
    '((consult-imenu-functions . ?f)
      (consult-imenu-variables . ?v)
      (consult-imenu-packages . ?p)
      (consult-imenu-types . ?t)
      (consult-imenu-macros . ?m)))

  ;; Add initial narrowing hook
  (defun consult-initial-narrow ()
    (when-let (key (alist-get this-command consult-initial-narrow-config))
      (setq unread-command-events (append unread-command-events (list key 32)))))
  (add-hook 'minibuffer-setup-hook #'consult-initial-narrow))

(use-package lsp-mode
  :commands lsp
  :config
  (setq gc-cons-threshold 1600000)
  (setq read-process-output-max (* 1024 1024)))
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  ;; (setf lsp-ui-doc-mode -1)
  ;; (setf (alist-get 'width lsp-ui-doc-frame-parameters) 80)
  )
;; (use-package consult-lsp
;; ;;  :after (consult lsp)
;;   )
(use-package marginalia
  :init
  (marginalia-mode +1))
(use-package ctrlf)

;;(use-package amx)

(use-package anzu
  :straight (anzu :type git
			      :host github
			      :repo "Overdr0ne/anzu")
  :config
  (global-anzu-mode +1))
;; (use-package iedit)
(use-package doom-modeline
  :init
  (setq doom-modeline-height 1)
  (doom-modeline-mode 1)
  (setq doom-modeline-minor-modes t))
;; (use-package shrink-path)

(use-package expand-region)

;; (use-package kbd-mode)

;; (use-package flycheck
;; ;;  :after (dash pkg-info let-alist seq)
;;   :config
;;   (setq flycheck-emacs-lisp-load-path 'inherit)
;;   (setq flycheck-indication-mode 'right-fringe)
;;   (setq flycheck-check-syntax-automatically '(idle-change new-line mode-enabled))
;;   (add-hook 'flycheck-error-list-mode-hook visual-line-mode))

;;(use-package evil
;;  :init
;;  (setq evil-want-integration t)
;;  (setq evil-want-keybinding nil)
;;  (setq evil-want-Y-yank-to-eol t)
;;  (setq evil-disable-insert-state-bindings t)
;;  ;; Don't let evil-collection interfere with certain keys
;;  (setq evil-collection-key-blacklist
;;	    (list "<escape>"))
;;  (defun +default|disable-delete-selection-mode ()
;;    (delete-selection-mode -1))
;;  (add-hook 'evil-insert-state-entry-hook #'delete-selection-mode)
;;
;;  :config
;;  (evil-mode +1)
;;  ;; (advice-add 'evil-goto-mark :after #'(lambda () (evil-scroll-line-to-center (line-number-at-pos))))
;;  )
;;;;(use-package flyspell-lazy)
;;;;(use-package flyspell-correct)
;;;;(use-package flyspell-correct-popup)
;;;;(use-package flycheck-aspell)
;;;;(use-package flycheck-ledger)
;;;;(use-package flycheck-popup-tip)
;;;;(use-package langtool)

;;;;;;(use-package forge)
;;;;(use-package diff-hl
;;;;  :config
;;;;  (global-diff-hl-mode))
;;;;(use-package git-modes)

(use-package magit
  :config
  (setf magit-buffer-log-args '("-n256" "--color" "--decorate" "--graph"))
  )
;;;;(use-package magit-gitflow)
;;;;;; (use-package magithub)
;;;;(use-package git-timemachine)
;; (use-package git-gutter-fringe
;;   :config
;;   (if (fboundp 'fringe-mode) (fringe-mode '8))

;;   ;; places the git gutter outside the margins.
;;   (setq-default fringes-outside-margins t)
;;   ;; thin fringe bitmaps
;;   (define-fringe-bitmap 'git-gutter-fr:added [224]
;;     nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:modified [224]
;;     nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
;;     nil nil 'bottom))

(use-package format-all)

(use-package dts-mode
  :config
  (add-to-list 'auto-mode-alist '("defconfig\\'" . dts-mode))
  (add-to-list 'auto-mode-alist '("defconfig\\'" . dts-mode))
  (setq auto-mode-alist (append auto-mode-alist '(("defconfig\\'" . dts-mode)
						                          ("\\.its" . dts-mode)))))
(use-package kconfig-mode)
;;;;(use-package yaml-mode
;;;;  :config
;;;;  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))
;;;;
;;;;(use-package yasnippet
;;;;  :config
;;;;  (yas-global-mode))
;;;;(use-package auto-yasnippet)
;;;;(use-package yasnippet-snippets)
;;;;
;;;;;;; langs
;;;;(use-package ccls
;;;;  :config
;;;;  (add-to-list 'lsp-enabled-clients 'ccls))
;;;;
;;;;(use-package pyvenv)
;;;;(use-package pydoc
;;;;  :straight (pydoc :type git
;;;;                   :host github
;;;;                   :branch "master"
;;;;                   :repo "Overdr0ne/pydoc"))
;;;;(use-package lsp-jedi
;;;;  :config
;;;;  (with-eval-after-load "lsp-mode"
;;;;    (add-to-list 'lsp-disabled-clients 'pyls)
;;;;    (add-to-list 'lsp-enabled-clients 'jedi))
;;;;  ;; (setf lsp-ui-doc-mode -1)
;;;;  )
;;;;
(use-package adaptive-wrap)
;;;;(use-package evil-tex)
;;;;
;;;;(use-package markdown-toc)
;;;;
;;;;(use-package org
;;;;  :config
;;;;  (setq org-directory "~/notes")
;;;;  (setq org-agenda-files '("~/notes"))
;;;;  (setq org-default-notes-file (concat org-directory "/default.org")))
;;;;;; (use-package evil-org
;;;;;;   :config
;;;;;;   (add-hook 'org-mode-hook 'evil-org-mode)
;;;;;;   (add-hook 'evil-org-mode-hook
;;;;;;             (lambda ()
;;;;;;               (evil-org-set-key-theme)))
;;;;;;   (require 'evil-org-agenda)
;;;;;;   (evil-org-agenda-set-keys))
;;;;(use-package toc-org)
;;;;(use-package org-superstar
;;;;  :config
;;;;  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
;;;;;;(use-package org-ref)
;;;;;; (use-package org-roam
;;;;;;   :init
;;;;;;   (setq org-roam-directory "~/notes")
;;;;;;   (add-hook 'after-init-hook 'org-roam-mode))
;;
;;;;(use-package web-mode)
(use-package rainbow-mode)
;;;;(use-package sass-mode)
;;;;(use-package slim-mode)
;;;;(use-package nix-mode)
;;;;(use-package racket-mode)

(use-package rainbow-delimiters)

;;(use-package cmake-mode)
;;(use-package modern-cpp-font-lock)
;;(use-package demangle-mode)
;;
;;(use-package csv-mode)
;;
;;(use-package py-isort)
;;(use-package pyimport)
;;
;;;;(use-package bash-completion
;;;;  :config
;;;;  (autoload 'bash-completion-dynamic-complete
;;;;          "bash-completion"
;;;;          "BASH completion hook")
;;;;  (add-hook 'shell-dynamic-complete-functions
;;;;            'bash-completion-dynamic-complete))
;;
;;(use-package request)
;;
;;(use-package treemacs)
;;(use-package treemacs-perspective)
;;(use-package treemacs-projectile)

(use-package vi-tilde-fringe
  :config
  (global-vi-tilde-fringe-mode +1))

;;(use-package multi-compile
;;  :config
;;  (setq multi-compile-alist '(
;;                              (c-mode . (("build" . "gcc -g *.c"))))))
;;
;;(use-package browse-kill-ring)
;;(use-package clipmon
;;  :config
;;  (clipmon-mode-start))
;;
;;(use-package multiple-cursors)
;;
;;(use-package cider)
;;
;;;;(use-package w3m)
;;
;;(use-package interaction-log)

;;;; (use-package workgroups
;;;;   :after (radix-tree))
(use-package perspective
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  ;; :config
  ;; (persp-mode +1)
  )

;; (use-package persp-projectile
;;  ;;  :after (persp projectile)
;;  )

;;(use-package visual-fill-column)
;;
;;;;;; (use-package find-file-in-project)
;;;;
;;;;;; (use-package bookmark+)
;;;;
;;;;(use-package system-packages
;;;;  :config
;;;;  (add-to-list 'system-packages-supported-package-managers
;;;;	           '(pacaur .
;;;;                        ((default-sudo . nil)
;;;;                         (install . "pacaur -S")
;;;;                         (search . "pacaur -Ss")
;;;;                         (uninstall . "pacaur -Rs")
;;;;                         (update . "pacaur -Syu")
;;;;                         (clean-cache . "pacaur -Sc")
;;;;                         (log . "cat /var/log/pacman.log")
;;;;                         (get-info . "pacaur -Qi")
;;;;                         (get-info-remote . "pacaur -Si")
;;;;                         (list-files-provided-by . "pacaur -Ql")
;;;;                         (verify-all-packages . "pacaur -Qkk")
;;;;                         (verify-all-dependencies . "pacaur -Dk")
;;;;                         (remove-orphaned . "pacaur -Rns $(pacman -Qtdq)")
;;;;                         (list-installed-packages . "pacaur -Qe")
;;;;                         (list-installed-packages-all . "pacaur -Q")
;;;;                         (list-dependencies-of . "pacaur -Qi")
;;;;                         (noconfirm . "--noconfirm"))))
;;;;  (setq system-packages-use-sudo nil)
;;;;  (setq system-packages-package-manager 'pacaur))
;;;;;; (use-package arch-packer)
;;;;
;;(use-package web-search)
;;
(use-package paredit)
;; (use-package macrostep)
(use-package smartparens)
;; (use-package smartparens
;;   :config
;;   (require 'smartparens-config))

;;;;(use-package lispy)
;;
;;(use-package clojure-mode)
;;(use-package elein)
;;
;;(use-package systemd)
;;
(use-package dired
  :straight (:type built-in)
  :config
  (setf dired-listing-switches "-alhF"))
(use-package diredfl
;;  :after (dired)
  :config
  (diredfl-global-mode))
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
;;(use-package dired-ranger)
;;(use-package dired-filter)
;;(use-package dired+
;;  :init
;;  (setq diredp-hide-details-initially-flag nil)
;;  (add-hook 'dired-mode-hook 'hl-line-mode))
;;(use-package dired-du)
;;(use-package dired-subtree
;;  :config
;;  (setf dired-subtree-use-backgrounds nil))
;;(use-package dired-filter)
;;(use-package dired-narrow)

(use-package highlight)
(use-package highlight-defined
  :config
  (highlight-defined-mode +1))

;;;;(use-package deft
;;;;  :config
;;;;  (add-to-list 'evil-insert-state-modes 'deft-mode)
;;;;  (setq deft-directory "~/notes"))
;;;;
;;;;(use-package auto-compile
;;;;  :init
;;;;  (setq load-prefer-newer t)
;;;;  :config
;;;;  (auto-compile-on-load-mode)
;;;;  (auto-compile-on-save-mode))
;;;;
;;;;(use-package all-the-icons-ibuffer
;;;;  :config
;;;;  (all-the-icons-ibuffer-mode))
;;;;
;;;;(use-package rcirc
;;;;  :config
;;;;  (rcirc-track-minor-mode t)
;;;;  (add-hook 'rcirc-mode-hook (lambda ()
;;;;                               (flyspell-mode 1)
;;;;                               (rcirc-omit-mode)))
;;;;
;;;;  (setq rcirc-server-alist
;;;;        '(("irc.libera.chat" :channels ("#emacs"))))
;;;;  (evil-set-initial-state 'rcirc-mode 'insert)
;;;;  (setq rcirc-buffer-maximum-lines 1000)
;;;;  (setq rcirc-default-nick "Overdr0ne")
;;;;  (setq rcirc-default-user-name "Overdr0ne")
;;;;  (setq rcirc-log-flag t))
;;;;
;;;;(use-package explain-pause-mode)
;;;;
;;;;;; (use-package daemons)
;;;;
;;;;;; (use-package pretty-mode
;;;;;;   :config
;;;;;;   (global-pretty-mode t)
;;;;;;   (pretty-deactivate-groups
;;;;;;    '(:equality :ordering :ordering-double :ordering-triple
;;;;;; 			   :arrows :arrows-twoheaded :punctuation
;;;;;; 			   :logic :sets))
;;;;;;   (pretty-activate-groups
;;;;;;    '(:sub-and-superscripts :greek :arithmetic-nary)))
;;
;;(use-package names)
;;
;;;; (use-package sx
;;;;  :config
;;;;  (evil-set-initial-state 'sx-question-list-mode 'emacs))
;;
;;(use-package multiple-cursors)
;;;;(use-package evil-mc)
;;;;
;;;;(use-package quelpa)
;;;;
;;;;(use-package bbdb
;;;;  :config
;;;;  (setq
;;;;   bbdb-file "~/.bbdb"
;;;;   bbdb-offer-save 'auto
;;;;   bbdb-notice-auto-save-file t
;;;;   bbdb-expand-mail-aliases t
;;;;   bbdb-canonicalize-redundant-nets-p t
;;;;   bbdb-always-add-addresses t
;;;;   bbdb-complete-name-allow-cycling t))
;;;;
;;;;(use-package all-the-icons-gnus
;;;;  :config
;;;;  (all-the-icons-gnus-setup))
;;;;(use-package gnus
;;;;  :config
;;;;  (setq user-mail-address "scmorris.dev@gmail.com"
;;;;        user-full-name "Sam")
;;;;  (setq gnus-asynchronous t)
;;;;  (setq gnus-use-article-prefetch 15)
;;;;
;;;;  (setq gnus-select-method '(nnnil ""))
;;;;  (setq gnus-secondary-select-methods
;;;;        '((nnimap "Mail"
;;;;                  (nnimap-address "localhost")
;;;;                  (nnimap-stream network)
;;;;                  (nnimap-authenticator login)
;;;;                  (nnir-search-engine imap))
;;;;          (nntp "news.gwene.org")))
;;;;  (setq mail-user-agent 'gnus-user-agent)
;;;;  (setq read-mail-command 'gnus)
;;;;
;;;;  (setq nnir-method-default-engines
;;;;        '((nnmaildir . notmuch)))
;;;;
;;;;  ;; threading
;;;;  (setq gnus-thread-sort-functions
;;;;        '(gnus-thread-sort-by-most-recent-number
;;;;          gnus-thread-sort-by-subject
;;;;          (not gnus-thread-sort-by-total-score)
;;;;          gnus-thread-sort-by-most-recent-date))
;;;;  (setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
;;;;  (setq gnus-summary-make-false-root-always 'adopt)
;;;;
;;;;  ;; groups
;;;;  (setq gnus-group-line-format "%M%p%P%5y:%B%(%g%)\n")
;;;;  (setq gnus-group-mode-line-format "%%b")
;;;;  (add-hook 'gnus-group-mode-hook 'hl-line-mode)
;;;;  (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
;;;;  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;;;;  (setq gnus-parameters
;;;;        '((".."
;;;;           (display . 200))
;;;;          ("nnimap\\\\..*"
;;;;           (display . 200))
;;;;          ("list\\..*"
;;;;           (total-expire . t)
;;;;           (broken-reply-to . t))))
;;;;
;;;;  ;; summary
;;;;  (add-hook 'gnus-summary-mode-hook 'hl-line-mode)
;;;;  (setq gnus-auto-select-first nil)
;;;;  (setq gnus-summary-ignore-duplicates t)
;;;;  (setq gnus-suppress-duplicates t)
;;;;  (setq gnus-save-duplicate-list t)
;;;;  (setq gnus-summary-goto-unread nil)
;;;;  (setq gnus-summary-make-false-root 'adopt)
;;;;  (setq gnus-summary-thread-gathering-function
;;;;        'gnus-gather-threads-by-subject)
;;;;  (setq gnus-summary-gather-subject-limit 'fuzzy)
;;;;  (setq gnus-thread-sort-functions
;;;;        '((not gnus-thread-sort-by-date)
;;;;          (not gnus-thread-sort-by-number)))
;;;;  (setq gnus-subthread-sort-functions
;;;;        'gnus-thread-sort-by-date)
;;;;  (setq gnus-thread-hide-subtree nil)
;;;;  (setq gnus-thread-ignore-subject nil)
;;;;  (setq gnus-user-date-format-alist
;;;;        '(((gnus-seconds-today) . "Today at %R")
;;;;          ((+ (* 60 60 24) (gnus-seconds-today)) . "Yesterday, %R")
;;;;          (t . "%Y-%m-%d %R")))
;;;;
;;;;  ;; topics
;;;;  (setq gnus-topic-line-format "%i[ %(%{%n -- %A%}%) ]%v\n")
;;;;  ;; format
;;;;  (copy-face 'font-lock-variable-name-face 'gnus-face-6)
;;;;  (setq gnus-face-6 'gnus-face-6)
;;;;  (copy-face 'font-lock-warning-face 'gnus-face-7)
;;;;  (setq gnus-face-7 'gnus-face-7)
;;;;  (copy-face 'gnus-face-7 'gnus-summary-normal-unread)
;;;;  (copy-face 'font-lock-function-name-face 'gnus-face-8)
;;;;  (set-face-foreground 'gnus-face-8 "gray50")
;;;;  (setq gnus-face-8 'gnus-face-8)
;;;;  (copy-face 'font-lock-type-face 'gnus-face-9)
;;;;  (set-face-foreground 'gnus-face-9 "gray70")
;;;;  (setq gnus-face-9 'gnus-face-9)
;;;;  (setq gnus-summary-line-format
;;;;        (concat
;;;;         "%0{%U%R%z%}"
;;;;         "%3{│%}" "%1{%d%}" "%3{│%}"
;;;;         "  "
;;;;         "%4{%-20,20f%}"
;;;;         "  "
;;;;         "%3{│%}"
;;;;         " "
;;;;         "%1{%B%}"
;;;;         "%s\n"))
;;;;  (setq gnus-summary-line-format "%8{%4k│%}%8{%d│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{║%} %6{%B%} %S\n"
;;;;        gnus-sum-thread-tree-indent " "
;;;;        gnus-sum-thread-tree-root " ┏● "
;;;;        gnus-sum-thread-tree-false-root " ○ "
;;;;        gnus-sum-thread-tree-single-indent " ● "
;;;;        gnus-sum-thread-tree-leaf-with-other "┣━━► "
;;;;        gnus-sum-thread-tree-vertical "┃"
;;;;        gnus-sum-thread-tree-single-leaf "┗━━► "))
;;
;;(use-package wordnut)
;;
;;;;(use-package xwwp)
;;;;
;;;;;; (defun proced-settings ()
;;;;;;   (proced-toggle-auto-update))
;;;;;; (add-hook 'proced-mode-hook 'proced-settings)

;;(use-package calfw)

(use-package minions
  :custom
  (minions-mode-line-lighter "⚙")
  (minions-mode-line-delimiters nil)
  (minions-direct '(overwrite-mode parinfer-mode))
  :config
  (minions-mode))

;;;; (use-package undo-tree
;;;;   :init
;;;;   (global-undo-tree-mode))
;;
;;;; (use-package hyperbole
;;;;   :init
;;;;   (setf hkey-init nil))

(use-package which-key
  :config
  (which-key-mode))

;;;; (use-package sfs
;;;;   :straight (sfs :type git
;;;;                  :host github
;;;;                  :repo "Overdr0ne/sfs"
;;;;                  :branch "master"
;;;;                  :files ("sfs.el"
;;;;                          "sfs-recoll.el"
;;;;                          "sfs-tui.el"
;;;;                          "sfs-tag.el"
;;;;                          "sfs-reindex.el"
;;;;                          "service.py"
;;;;                          "evil-collection-sfs.el"))
;;;;   :config
;;;;   (add-to-list 'evil-insert-state-modes 'sfs-research-mode)
;;;;   (global-sfs-mode 1))

;; themes
;;(use-package load-theme-buffer-local)
;;(use-package nofrils-acme-theme)
;;;; (use-package plan9-theme)
;;;; (use-package cyberpunk-2019-theme
;;;;   :config
;;;;   (load-theme 'cyberpunk-2019 t))
;;;; (use-package acme-theme
;;;;   :config
;;;;   (load-theme 'acme t))
(use-package gruvbox-theme)
(use-package alect-themes)
;;;; (use-package adwaita
;;;;   :config
;;;;   (load-theme 'adwaita 't))
;;;; (use-package dichromacy
;;;;   :config
;;;;   (load-theme 'dichromacy 't))

(use-package dracula-theme
  :straight (dracula-theme :type git
			               :host github
			               :repo "Overdr0ne/emacs")
  :config
  (load-theme 'dracula t))
;;(use-package solarized-theme
;; ;; :config
;; ;; (load-theme 'solarized-light t)
;; )
;;(use-package spacemacs-theme
;; :straight (spacemacs-theme :type git
;;			                 :host github
;;			                 :repo "Overdr0ne/spacemacs-theme")
;; :defer t
;; ;; :init
;; ;; (load-theme 'spacemacs-light t)
;; )

;;;;(use-package mixed-pitch)
;;;;(use-package writeroom-mode)

(use-package multi-term)

(use-package imenu
  :config
  (setq imenu-generic-expression '(("Section"
                                    "^;;;"))))

(use-package disk-usage)

(use-package good-scroll
  :config
  (good-scroll-mode +1))

;;(use-package command-mode
;; :straight (command-mode :type git
;;                         :host github
;;                         :repo "Overdr0ne/command-mode"
;;                         :branch "master"))
;;
(use-package shelldon
  :straight (shelldon :type git
                      :host github
                      :repo "Overdr0ne/shelldon"
                      :branch "master"
                      :files ("shelldon.el"))
  :config
  (setf shell-command-switch "-ic")
  (setf enable-recursive-minibuffers t)
  ;; (add-to-list 'evil-normal-state-modes 'shelldon-mode)
  ;; (setenv "TERM" "eterm-color")
  (add-hook 'shelldon-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'shelldon-mode-hook #'(lambda () (view-mode -1)))
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (advice-add #'shelldon-output-history :around
	          (lambda (old-fn)
		        (let ((selectrum-should-sort nil))
		          (funcall old-fn)))))

;; (use-package emacs-application-framework
;;   :straight (emacs-application-framework :type git
;; 					 :host github
;; 					 :repo "manateelazycat/emacs-application-framework"
;; 					 :files ("*")))

;; (use-package webkit
;;   :straight (webkit :type git
;; 		    :host github
;; 		    :repo "akirakyle/emacs-webkit"
;; 		    :branch "main"
;; 		    :files (:defaults "*.js" "*.css" "*.so")
;; 		    :build ("make")))
;;
(use-package corfu
  :init
  (defun corfu-setup-advice ()
    (defvar corfu-mode-map-alist)
    (setq corfu-mode-map-alist `((completion-in-region-mode . ,corfu-map)))
    (add-to-list 'emulation-mode-map-alists 'corfu-mode-map-alist))
  (setf corfu-quit-at-boundary t)
  ;; (advice-add 'corfu--setup :before #'corfu-setup-advice)
  :config
  (add-hook 'prog-mode-hook 'corfu-mode))
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

(use-package highlight-indentation
  :straight (highlight-indentation :type git
                                   :host github
                                   :repo "antonj/Highlight-Indentation-for-Emacs"
                                   :branch "master"))

(use-package indent-guide
  :straight (indent-guide :type git
                          :host github
                          :repo "zk-phi/indent-guide"
                          :branch "master"))

;;(use-package transpose-frame)

(use-package buffer-move)

;;;; (use-package sclang)
;;;; (use-package sclang
;;;;   :straight (sclang :type git
;;;;                     :host github
;;;;                     :repo "Overdr0ne/scel"
;;;;                     :branch "master"))
;;;; (setq sclang-help-path '("/home/sam/.local/share/SuperCollider/Help"))
;;(use-package sclang-extensions)
;;(use-package sclang-snippets)
;;
;;(use-package haskell-mode)

;; (use-package gumshoe
;;   :straight (gumshoe :type git
;;                      :host github
;;                      :repo "Overdr0ne/gumshoe"
;;                      :branch "master")
;;   :init
;;   (global-gumshoe-persp-mode 1)
;;   (setf gumshoe-slot-schema '(time perspective buffer position line))
;;   (advice-add #'gumshoe-peruse-globally :around
;; 	          (lambda (old-fn)
;; 		        (let ((selectrum-should-sort nil))
;; 		          (funcall old-fn))))
;;   (advice-add #'gumshoe-peruse-in-persp :around
;; 	          (lambda (old-fn)
;; 		        (let ((selectrum-should-sort nil))
;; 		          (funcall old-fn))))
;;   (advice-add #'gumshoe-peruse-in-buffer :around
;; 	          (lambda (old-fn)
;; 		        (let ((selectrum-should-sort nil))
;; 		          (funcall old-fn)))))

(use-package embark
  :config
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))
;;(use-package embark-consult
;; :hook
;; (embark-collect-mode . consult-preview-at-point-mode))
;;
;;;;;; (use-package better-jumper)
;;;;
;;;;;; (use-package esup)
;;
;;(use-package package-lint)
;;
;;;;;; (use-package bufler)
;;
;;(use-package burly)

;; (use-package prism
;;   :straight (prism :type git
;;                    :host github
;;                    :branch "master"
;;                    :repo "Overdr0ne/prism.el")
;;   :config
;;   (setf prism-num-faces 30
;; 	    prism-color-attribute :background
;; 	    prism-colors (list "maroon" "violet" "blue violet" "blue" "light sea green" "green" "yellow green" "yellow" "orange" "red")
;; 	    prism-desaturations (list 0 0 0)
;; 	    prism-lightens (list 0 0 0)
;; 	    prism-opacities (list 10 30 90)
;; 	    prism-comments-fn (lambda (color) color)
;; 	    prism-strings-fn (lambda (color) color)
;; 	    prism-parens-fn (lambda (color) color))
;;   ;; (prism-save-colors)
;;  )

;;;;;; (load "~/src/winblows/winblows.el")

(use-package loccur)

(use-package bitbake
  :straight (bitbake :type git
                     :host github
                     :repo "Overdr0ne/bitbake-el"
                     :branch "master"
                     :build (:not compile))
  :config
  (setf bitbake-flash-device "/dev/mmcblk0")
  ;; (setf bitbake-poky-directory "/home/sam/workspaces/dtech/layers")
  ;; (setf bitbake-build-directory "/home/sam/workspaces/dtech/build")
  (setf bitbake-poky-directory "/home/sam/workspaces/impinj")
  (setf bitbake-build-directory "/home/sam/workspaces/impinj/build"))

;;;;;; (use-package exec-path-from-shell)
;;;;
;;;;(use-package tmm
;;;;  :config
;;;;  (setf tmm-completion-prompt "")
;;;;  (setf tmm-mid-prompt ":"))
;;;;
;;;;(use-package docker
;;;; :config
;;;; (setf docker-image-run-default-args '("-i" "-t" "--rm")))
;;
;;(use-package dockerfile-mode
;; :config
;; (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))
;;
;;;;;; (use-package keychain-environment
;;;;;;   :straight (keychain-environment :type git
;;;;;; 				  :host github
;;;;;; 				  :repo "tarsius/keychain-environment"
;;;;;; 				  :branch "master"))
;;;;;; (use-package wrap-region-mode
;;;;;;   :straight (wrap-region-mode :type git
;;;;;; 			      :host github
;;;;;; 			      :repo "rejeep/wrap-region.el"
;;;;;; 			      :branch "master")
;;;;;;   )

(use-package ediff
  :straight (:type built-in)
  :init
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (add-hook 'ediff-mode-hook #'ediff-reload-keymap)
  (defvar my-ediff-last-windows nil)

  (defun my-store-pre-ediff-winconfig ()
    (setq my-ediff-last-windows (current-window-configuration)))

  (defun my-restore-pre-ediff-winconfig ()
    (set-window-configuration my-ediff-last-windows))

  (add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
  (add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig))

;;;;(use-package elgrep)

;; (use-package rg)

(use-package slink
  :straight (slink :type git
                   :host github
                   :repo "Overdr0ne/slink"
                   :branch "main"))

;; (use-package geiser)
;; (use-package geiser-guile)
;; (use-package bui)
;; (use-package edit-indirect)
;; (use-package build-farm)
;; (use-package magit-popup)
;; (use-package guix)

;;(use-package recentf
;; :straight (recentf :type built-in)
;; :init
;; (recentf-mode +1))
;; (use-package evil)
;; (use-package evil
;;   :straight (evil :build (:not compile)))
;; (use-package evil
;;   :straight (evil :build (:not compile))
;;   :init
;;   (setq evil-want-integration t)
;;   (setq evil-want-keybinding nil)
;;   (setq evil-want-Y-yank-to-eol t)
;;   (setq evil-disable-insert-state-bindings t)
;;   ;; Don't let evil-collection interfere with certain keys
;;   ;; (setq evil-collection-key-blacklist (list "<escape>"))
;;   ;;:config
;;   ;;(evil-mode +1)
;;   )
;; (use-package evil-collection
;;   :straight (evil-collection :build (:not compile))
;;   :init
;;   (setq evil-collection-want-unimpaired-p nil)
;;   :config
;;   (evil-collection-init)
;;   ;; (global-evil-collection-unimpaired-mode -1)
;;   )
;; ;;(use-package evil-anzu)
;; (use-package evil-escape
;;   :config
;;   (setq evil-escape-key-sequence nil))
;; (use-package evil-goggles
;;   :config
;;   (evil-goggles-mode)
;;   (evil-goggles-use-diff-faces))
;;(use-package evil-easymotion)
;; (use-package evil-matchit
;;   :config
;;   (global-evil-matchit-mode 1))
;; ;; (use-package evil-snipe
;; ;;   ;; :config
;; ;;   ;; (push 'ibuffer-mode evil-snipe-disabled-modes)
;; ;;   ;; (evil-snipe-mode +1)
;; ;;   )
;; ;;(use-package evil-nerd-commenter)
;; (use-package evil-traces
;;   :config
;;   (evil-traces-mode))
;; (use-package evil-commentary
;;   :config
;;   (evil-commentary-mode))
;; (use-package evil-surround
;;   :config
;;   (setq-default evil-surround-pairs-alist '((?\( . ("(" . ")"))
;; 					                        (?\[ . ("[" . "]"))
;; 					                        (?\{ . ("{" . "}"))

;; 					                        (?\) . ("( " . " )"))
;; 					                        (?\] . ("[ " . " ]"))
;; 					                        (?\} . ("{ " . " }"))

;; 					                        (?# . ("#{" . "}"))
;; 					                        (?b . ("(" . ")"))
;; 					                        (?B . ("{" . "}"))
;; 					                        (?> . ("<" . ">"))
;; 					                        (?t . evil-surround-read-tag)
;; 					                        (?< . evil-surround-read-tag)
;; 					                        (?\C-f . evil-surround-prefix-function)
;; 					                        (?f . evil-surround-function))))
;; (use-package evil-cleverparens
;;   :init
;;   (setq evil-cleverparens-use-regular-insert 't))

(load "~/src/evim/evim.el")

(use-package tempel
  :config
  (add-to-list 'completion-at-point-functions #'tempel-expand)
  (setf tempel-path "~/.emacs.d/overdr0ne/template.el")
  )

(use-package cmake-mode)

(use-package yaml-mode)

(use-package meson-mode)

;; (use-package mode-minder
;;   :straight (mode-minder :type git
;;                          :host github
;;                          :repo "jdtsmith/mode-minder"))

(provide '+modules)
;;; +modules.el ends here
