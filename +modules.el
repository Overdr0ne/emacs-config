;;; ~/.emacs.d/+use-package.el -*- lexical-binding: t; -*-

;; (use-package no-littering)

(use-package general)

(use-package tree-sitter-langs)
(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package erc)

(use-package helpful)

(use-package winner
  :init
  (winner-mode t))

(use-package selectrum
  :config
  (selectrum-mode +1))
(use-package consult
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  (defalias #'consult-imenu-variables #'consult-imenu)
  (defalias #'consult-imenu-functions #'consult-imenu)
  (defalias #'consult-imenu-macros #'consult-imenu)
  (defalias #'consult-imenu-packages #'consult-imenu)
  (defalias #'consult-imenu-types #'consult-imenu)
  ;; Configure initial narrowing per command
  (setq consult-initial-narrow-config
    '((consult-imenu-functions . ?f)
      (consult-imenu-variables . ?v)
      (consult-imenu-packages . ?p)
      (consult-imenu-types . ?t)
      (consult-imenu-macros . ?m)))

  ;; Add initial narrowing hook
  (defun consult-initial-narrow ()
    (when-let (key (alist-get this-command consult-initial-narrow-config))
      (setq unread-command-events (append unread-command-events (list key 32)))))
  (add-hook 'minibuffer-setup-hook #'consult-initial-narrow)
  (remove-hook 'minibuffer-setup-hook #'consult-initial-narrow))
(use-package marginalia
  :init
  (marginalia-mode))
(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package amx)

(use-package anzu
  :config
  (global-anzu-mode +1))
(use-package doom-modeline
  :init
  (setq doom-modeline-height 1)
  (doom-modeline-mode 1)
  (setq doom-modeline-minor-modes t))
;; (use-package shrink-path)

(use-package expand-region)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1)
  ;; (advice-add 'evil-goto-mark :after #'(lambda () (evil-scroll-line-to-center (line-number-at-pos))))
  )
(use-package evil-collection
  :config
  (evil-collection-init))
(use-package evil-anzu)
(use-package evil-escape
  :config
  (setq evil-escape-key-sequence nil))
(use-package evil-goggles
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-nerd-commenter)
(use-package evil-snipe
  :config
  (evil-snipe-mode +1))
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))
;;(use-package evil-embrace)
(use-package evil-traces
  :config
  (evil-traces-mode))

(use-package flyspell-lazy)
(use-package flyspell-correct)
(use-package flyspell-correct-popup)
(use-package flycheck
  :config
  (setq flycheck-indication-mode 'right-fringe))
(use-package flycheck-aspell)
(use-package flycheck-ledger)
(use-package flycheck-popup-tip)
(use-package langtool)

;; (use-package transient)

(use-package forge)
(use-package magit)
(use-package magit-gitflow)
(use-package magithub)
(use-package git-timemachine)
(use-package git-gutter-fringe
  :config
  (if (fboundp 'fringe-mode) (fringe-mode '8))

  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))

(use-package format-all)

(use-package dts-mode)

(use-package yasnippet
  :config
  (yas-global-mode))
(use-package auto-yasnippet)
(use-package yasnippet-snippets)

;;; langs
(use-package lsp-mode
  :commands lsp
  :config
  (setq gc-cons-threshold 1600000)
  (setq read-process-output-max (* 1024 1024)))
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setf (alist-get 'width lsp-ui-doc-frame-parameters) 80))

(use-package pyvenv)

(use-package adaptive-wrap)
(use-package evil-tex)

(use-package markdown-toc)

(use-package org)
(use-package evil-org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
(use-package toc-org)
(use-package org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
;;(use-package org-ref)
;; (use-package org-roam
;;   :init
;;   (setq org-roam-directory "~/notes")
;;   (add-hook 'after-init-hook 'org-roam-mode))

(use-package web-mode)
(use-package rainbow-mode)
(use-package sass-mode)
(use-package slim-mode)
(use-package nix-mode)
(use-package racket-mode)

(use-package rainbow-delimiters)

(use-package cmake-mode)
(use-package modern-cpp-font-lock)
(use-package demangle-mode)

(use-package csv-mode)

(use-package py-isort)
(use-package pyimport)

(use-package bash-completion
  :config
  (bash-completion-setup))

(use-package request)

(use-package treemacs)
(use-package treemacs-perspective)
(use-package treemacs-projectile)

(use-package vi-tilde-fringe)

(use-package multi-compile
  :config
  (setq multi-compile-alist '(
                              (c-mode . (("build" . "gcc -g *.c"))))))

(use-package browse-kill-ring)
(use-package clipmon
  :init
  (clipmon-mode-start))

(use-package multiple-cursors)

(use-package cider)

(use-package w3m)

(use-package interaction-log)

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-easymotion)

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package evil-snipe
  :config
  (push 'ibuffer-mode evil-snipe-disabled-modes))

(use-package projectile
  :config
  (add-to-list 'projectile-globally-ignored-directories "build")
  (defun sam-projectile-ibuffer-by-project (project-root)
    "Open an IBuffer window showing all buffers in PROJECT-ROOT."
    (let ((project-name (funcall projectile-project-name-function project-root)))
      (ibuffer t (format "*%s Buffers*" project-name)
               (list (cons 'projectile-files project-root)) nil t)))

  (defun sam-projectile-ibuffer (prompt-for-project)
    "Open an IBuffer window showing all buffers in the current project.

Let user choose another project when PROMPT-FOR-PROJECT is supplied."
    (interactive "P")
    (let ((project-root (if prompt-for-project
                            (projectile-completing-read
                             "Project name: "
                             (projectile-relevant-known-projects))
                          (projectile-project-root))))

      (sam-projectile-ibuffer-by-project project-root)))
  (defun sam-projectile-vc-or-dired ()
    (interactive)
    (if (not (string-equal (projectile-project-vcs) "none"))
        (projectile-vc)
      (dired default-directory)
      ))
  (setq projectile-switch-project-action #'sam-projectile-vc-or-dired)
  (add-hook 'projectile-after-switch-project-hook 'delete-other-windows))

(use-package workgroups)

(use-package perspective
  :config
  (persp-mode))

(use-package persp-projectile
  :config
  (add-hook 'projectile-after-switch-project-hook 'delete-other-windows)
  ;; (defun sam-dired-if-not-vc ()
  ;;   (when (string-equal (projectile-project-vcs) "none")
  ;;     (projectile-dired)))
  ;; (add-hook 'projectile-after-switch-project-hook 'sam-dired-if-not-vc)

  (defun sam-projectile-vc-or-dired ()
    (interactive)
    (if (not (string-equal (projectile-project-vcs) "none"))
        (projectile-vc)
      (dired default-directory)))
  (setq projectile-switch-project-action #'sam-projectile-vc-or-dired))

(use-package visual-fill-column)

(use-package find-file-in-project)

(use-package bookmark+)

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
  (setq system-packages-use-sudo nil)
  (setq system-packages-package-manager 'pacaur))
;; (use-package arch-packer)

(use-package web-search)

(use-package load-theme-buffer-local)
;; (use-package nofrils-acme-theme)
;; (use-package plan9-theme)
;; (use-package cyberpunk-2019-theme
;;   :config
;;   (load-theme 'cyberpunk-2019 t))
;; (use-package acme-theme
;;   :config
;;   (load-theme 'acme t))
(use-package gruvbox-theme)
;; :config
;; (load-theme 'gruvbox-light-soft t)
;; (use-package adwaita
;;   :config
;;   (load-theme 'adwaita 't))
;; (use-package dichromacy
;;   :config
;;   (load-theme 'dichromacy 't))

(use-package paredit)
(use-package macrostep)
(use-package lispy)
(use-package smartparens
  :config
  (require 'smartparens-config))

(use-package smart-tabs-mode)
(use-package smartparens)
(use-package lispy)
(use-package evil-cleverparens
  :init
  (setq evil-cleverparens-use-regular-insert 't))

(use-package clojure-mode)
(use-package elein)

(use-package systemd)

(use-package diredfl
  :config
  (diredfl-global-mode))
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
(use-package dired-ranger)
(use-package dired-filter)
(use-package dired+
  :init
  (setq diredp-hide-details-initially-flag nil)
  (add-hook 'dired-mode-hook 'hl-line-mode))
(use-package dired-du)
(use-package dired-subtree)
(use-package dired-filter)
(use-package dired-narrow)

(use-package highlight)
(use-package highlight-defined)

(use-package deft
  :config
  (add-to-list 'evil-insert-state-modes 'deft-mode)
  (setq deft-directory "~/notes"))

(use-package auto-compile
  :init
  (setq load-prefer-newer t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package all-the-icons-ibuffer
  :config
  (all-the-icons-ibuffer-mode))

(use-package rcirc
  :config
  (rcirc-track-minor-mode t)
  (add-hook 'rcirc-mode-hook (lambda ()
                               (flyspell-mode 1)
                               (rcirc-omit-mode)))

  (setq rcirc-server-alist
        '(("irc.libera.chat" :channels ("#emacs"))))
  (evil-set-initial-state 'rcirc-mode 'insert)
  (setq rcirc-buffer-maximum-lines 1000)
  (setq rcirc-default-nick "Overdr0ne")
  (setq rcirc-default-user-name "Overdr0ne")
  (setq rcirc-log-flag t))

(use-package explain-pause-mode)

(use-package daemons)

;; (use-package pretty-mode
;;   :config
;;   (global-pretty-mode t)
;;   (pretty-deactivate-groups
;;    '(:equality :ordering :ordering-double :ordering-triple
;; 			   :arrows :arrows-twoheaded :punctuation
;; 			   :logic :sets))
;;   (pretty-activate-groups
;;    '(:sub-and-superscripts :greek :arithmetic-nary)))

(use-package names)

(use-package sx
  :config
  (evil-set-initial-state 'sx-question-list-mode 'emacs))

(use-package multiple-cursors)
(use-package evil-mc)

(use-package quelpa)

(use-package bbdb
  :config
  (setq
   bbdb-file "~/.bbdb"
   bbdb-offer-save 'auto
   bbdb-notice-auto-save-file t
   bbdb-expand-mail-aliases t
   bbdb-canonicalize-redundant-nets-p t
   bbdb-always-add-addresses t
   bbdb-complete-name-allow-cycling t))

(use-package all-the-icons-gnus
  :config
  (all-the-icons-gnus-setup))
(use-package gnus
  :config
  (setq user-mail-address "scmorris.dev@gmail.com"
        user-full-name "Sam")
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch 15)

  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods
        '((nnimap "Mail"
                  (nnimap-address "localhost")
                  (nnimap-stream network)
                  (nnimap-authenticator login)
                  (nnir-search-engine imap))
          (nntp "news.gwene.org")))
  (setq mail-user-agent 'gnus-user-agent)
  (setq read-mail-command 'gnus)

  (setq nnir-method-default-engines
        '((nnmaildir . notmuch)))

  ;; threading
  (setq gnus-thread-sort-functions
        '(gnus-thread-sort-by-most-recent-number
          gnus-thread-sort-by-subject
          (not gnus-thread-sort-by-total-score)
          gnus-thread-sort-by-most-recent-date))
  (setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
  (setq gnus-summary-make-false-root-always 'adopt)

  ;; groups
  (setq gnus-group-line-format "%M%p%P%5y:%B%(%g%)\n")
  (setq gnus-group-mode-line-format "%%b")
  (add-hook 'gnus-group-mode-hook 'hl-line-mode)
  (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (setq gnus-parameters
        '((".."
           (display . 200))
          ("nnimap\\\\..*"
           (display . 200))
          ("list\\..*"
           (total-expire . t)
           (broken-reply-to . t))))

  ;; summary
  (add-hook 'gnus-summary-mode-hook 'hl-line-mode)
  (setq gnus-auto-select-first nil)
  (setq gnus-summary-ignore-duplicates t)
  (setq gnus-suppress-duplicates t)
  (setq gnus-save-duplicate-list t)
  (setq gnus-summary-goto-unread nil)
  (setq gnus-summary-make-false-root 'adopt)
  (setq gnus-summary-thread-gathering-function
        'gnus-gather-threads-by-subject)
  (setq gnus-summary-gather-subject-limit 'fuzzy)
  (setq gnus-thread-sort-functions
        '((not gnus-thread-sort-by-date)
          (not gnus-thread-sort-by-number)))
  (setq gnus-subthread-sort-functions
        'gnus-thread-sort-by-date)
  (setq gnus-thread-hide-subtree nil)
  (setq gnus-thread-ignore-subject nil)
  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "Today at %R")
          ((+ (* 60 60 24) (gnus-seconds-today)) . "Yesterday, %R")
          (t . "%Y-%m-%d %R")))

  ;; topics
  (setq gnus-topic-line-format "%i[ %(%{%n -- %A%}%) ]%v\n")
  ;; format
  (copy-face 'font-lock-variable-name-face 'gnus-face-6)
  (setq gnus-face-6 'gnus-face-6)
  (copy-face 'font-lock-warning-face 'gnus-face-7)
  (setq gnus-face-7 'gnus-face-7)
  (copy-face 'gnus-face-7 'gnus-summary-normal-unread)
  (copy-face 'font-lock-function-name-face 'gnus-face-8)
  (set-face-foreground 'gnus-face-8 "gray50")
  (setq gnus-face-8 'gnus-face-8)
  (copy-face 'font-lock-type-face 'gnus-face-9)
  (set-face-foreground 'gnus-face-9 "gray70")
  (setq gnus-face-9 'gnus-face-9)
  (setq gnus-summary-line-format
        (concat
         "%0{%U%R%z%}"
         "%3{│%}" "%1{%d%}" "%3{│%}"
         "  "
         "%4{%-20,20f%}"
         "  "
         "%3{│%}"
         " "
         "%1{%B%}"
         "%s\n"))
  (setq gnus-summary-line-format "%8{%4k│%}%8{%d│%}%9{%U%R%z%}%8{│%}%*%(%-23,23f%)%7{║%} %6{%B%} %S\n"
        gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-root " ┏● "
        gnus-sum-thread-tree-false-root " ○ "
        gnus-sum-thread-tree-single-indent " ● "
        gnus-sum-thread-tree-leaf-with-other "┣━━► "
        gnus-sum-thread-tree-vertical "┃"
        gnus-sum-thread-tree-single-leaf "┗━━► "))

(use-package wordnut)

(use-package xwwp)

;; (defun proced-settings ()
;;   (proced-toggle-auto-update))
;; (add-hook 'proced-mode-hook 'proced-settings)

(use-package calfw)

(use-package minions
  :custom
  (minions-mode-line-lighter "⚙")
  (minions-mode-line-delimiters nil)
  (minions-direct '(overwrite-mode parinfer-mode))
  :config
  (minions-mode))

(use-package undo-tree
  :init
  (global-undo-tree-mode))

;; (use-package hyperbole)

(use-package which-key
  :config
  (which-key-mode))

(use-package sfs
  :straight (sfs :type git
                 :host github
                 :repo "Overdr0ne/sfs"
                 :branch "master"
                 :files ("sfs.el"
                         "sfs-recoll.el"
                         "sfs-tui.el"
                         "sfs-tag.el"
                         "sfs-reindex.el"
                         "service.py"
                         "evil-collection-sfs.el"))
  :config
  (global-sfs-mode 1))

;;; themes
(use-package dracula-theme
  :config
  (load-theme 'dracula t))
(use-package solarized-theme
  ;; :config
  ;; (load-theme 'solarized-light t)
  )
(use-package spacemacs-theme
  :defer t
  ;; :init
  ;; (load-theme 'spacemacs-light t)
  )

(use-package mixed-pitch)
(use-package writeroom-mode)

(use-package multi-term)

(use-package imenu
  :config
  (setq imenu-generic-expression '(("Section"
                                    "^;;;"))))

(use-package disk-usage)

(use-package good-scroll
  :config
  (good-scroll-mode +1))

(use-package command-mode
  :straight (command-mode :type git
                          :host github
                          :repo "Overdr0ne/command-mode"
                          :branch "master"))

(use-package bash-completion
  :init
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete))

(use-package shelldon
  :straight (shelldon :type git
                      :host github
                      :repo "Overdr0ne/shelldon"
                      :branch "master"
                      :files ("shelldon.el"))
  :config
  ;; (require 'shelldon)
  (setq shell-command-switch "-ic")
  (add-to-list 'evil-normal-state-modes 'shell-mode)
  ;; (setenv "TERM" "eterm-color")
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t))

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

(use-package corfu
  :init
  (defun corfu-setup-advice ()
    (defvar corfu-mode-map-alist)
    (setq corfu-mode-map-alist `((completion-in-region-mode . ,corfu-map)))
    (add-to-list 'emulation-mode-map-alists 'corfu-mode-map-alist))
  ;; (advice-add 'corfu--setup :before #'corfu-setup-advice)
  :config
  (add-hook 'prog-mode-hook 'corfu-mode))

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

;; (use-package sclang)
;; (use-package sclang
;;   :straight (sclang :type git
;;                     :host github
;;                     :repo "Overdr0ne/scel"
;;                     :branch "master"))
;; (setq sclang-help-path '("/home/sam/.local/share/SuperCollider/Help"))
(use-package sclang-extensions)
(use-package sclang-snippets)

(use-package haskell-mode)

;; (use-package golden-ratio
;;   :config
;;   (golden-ratio-mode +1))

(use-package gumshoe
  :straight (gumshoe :type git
                     :host github
                     :repo "Overdr0ne/gumshoe"
                     :branch "master")
  :config
  (global-gumshoe-mode 1)
  (global-gumshoe-persp-mode 1)
  (global-gumshoe-buf-mode 1)
  (defun consult-gumshoe ()
    (interactive)
    (consult-global-mark (ring-elements (oref gumshoe--global-backlog log))))
  (defun consult-gumshoe-persp ()
    (interactive)
    (consult-global-mark (ring-elements (oref gumshoe--persp-backlog log))))
  (defun consult-gumshoe-buf ()
    (interactive)
    (consult-global-mark (ring-elements (oref gumshoe--buf-backlog log)))))

(use-package embark
  :config
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package better-jumper)

(use-package esup)

(provide '+modules)
;;; +modules.el ends here
