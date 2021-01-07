;;; ~/.emacs.d/+use-package.el -*- lexical-binding: t; -*-

(use-package general)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                          ;; (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5))))

(use-package erc)

(use-package elfeed)

(use-package helpful)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))
;;(use-package company-files)
;;(use-package company-dict)
;; (use-package company-box)

;; (use-package counsel-dash)
(use-package counsel-projectile
  :config
  (counsel-projectile-mode +1))

(use-package selectrum
  :config
  (selectrum-mode +1))
(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode +1))

;; (use-package smex)
(use-package amx)
(use-package all-the-icons-ivy-rich
  :init
  (all-the-icons-ivy-rich-mode 1))
(use-package ivy-rich
  :init (ivy-rich-mode 1))
(use-package historian)
(use-package ivy-historian)
(use-package all-the-icons-ivy
  :init
  (ivy-mode +1)
  (historian-mode +1)
  (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  :config
  (ivy-historian-mode +1))

(use-package swiper)

;;(use-package ivy-historian)
;; (use-package ivy-hydra)
;; (use-package ivy-xref)
;; (use-package amx)

;; (use-package wgrep)

(use-package anzu
  :config
  (global-anzu-mode +1))
(use-package doom-modeline
  :init
  (setq doom-modeline-height 1)
  (doom-modeline-mode 1)
  (setq doom-modeline-minor-modes t))
;; (use-package shrink-path)

(use-package helm
  :config
  (setq projectile-completion-system 'default))
(use-package helm-descbinds)
(use-package helm-rg)
(use-package helm-c-yasnippet)
(use-package helm-company)
(use-package helm-describe-modes)
(use-package helm-projectile)
(use-package swiper-helm)
(use-package helm-flx)
(use-package helm-lsp)
(use-package helm-xref)
(use-package helm-dash)
(use-package helm-taskswitch)

(use-package expand-region)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
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
;; (use-package magithub
;;   :after magit
;;   :config
;;   (magithub-feature-autoinject t)
;;   (setq magithub-clone-default-directory "~/src"))
(use-package git-gutter-fringe
  :config
  (if (fboundp 'fringe-mode) (fringe-mode '4))

  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  ;; Please adjust fringe width if your own sign is too big.
  ;; (setq-default left-fringe-width  8)
  ;; (setq-default right-fringe-width 8)

  ;; (fringe-helper-define 'git-gutter-fr:added nil
  ;;   ".XXXXXX."
  ;;   "XXXXXXXX"
  ;;   "XXX..XXX"
  ;;   "XX....XX"
  ;;   "XXXXXXXX"
  ;;   "XXXXXXXX"
  ;;   "XX....XX"
  ;;   "XX....XX")

  ;; (fringe-helper-define 'git-gutter-fr:deleted nil
  ;;   "XXXXXX.."
  ;;   "XXXXXXX."
  ;;   "XX...XXX"
  ;;   "XX....XX"
  ;;   "XX....XX"
  ;;   "XX...XXX"
  ;;   "XXXXXXX."
  ;;   "XXXXXX..")

  ;; (fringe-helper-define 'git-gutter-fr:modified nil
  ;;   "XXXXXXXX"
  ;;   "XXXXXXXX"
  ;;   "X..XX..X"
  ;;   "X..XX..X"
  ;;   "X..XX..X"
  ;;   "X..XX..X"
  ;;   "X..XX..X"
  ;;   "X..XX..X")
  )

(use-package format-all)

(use-package dts-mode)

(use-package yasnippet
  :config
  (yas-global-mode))
(use-package auto-yasnippet)
(use-package yasnippet-snippets)

;;; langs
(use-package lsp-mode :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
(use-package lsp-ivy)

(use-package adaptive-wrap)
(use-package evil-tex)

;;(package! evil-markdown)
(use-package markdown-toc)

(use-package org)
;;(use-package helm-org)
(use-package evil-org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme))))
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

(use-package rainbow-delimiters)

(use-package cmake-mode)
(use-package ccls
  :init
  (add-hook 'elisp-mode-hook 'lsp)
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp))
(use-package modern-cpp-font-lock)
(use-package demangle-mode)

(use-package csv-mode)

(use-package py-isort)
(use-package pyimport)

(use-package company
  :config
  (setq company-auto-commit t)
  (setq company-auto-commit-chars " ")
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:background "navajo white"))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))
  (add-to-list 'company-backends 'company-files 'company-yasnippet)
  (general-add-hook 'emacs-lisp-mode-hook #'(lambda () (add-to-list (make-local-variable 'company-backends) 'company-elisp)))
  )

(use-package treemacs)
(use-package treemacs-perspective)
(use-package treemacs-projectile)

(use-package vi-tilde-fringe)

;; (use-package multi-compile)
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

;; sclang installed with emacs on arch
(use-package sclang)
(setq sclang-help-path '("/home/sam/.local/share/SuperCollider/Help"))
(use-package sclang-extensions)
(use-package sclang-snippets)

(use-package w3m)

(use-package interaction-log)

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-easymotion)
;; :config
;; (setq evil-cp-additional-movement-keys
;;       '(("L" . evil-cp-forward-sexp)
;;         ("H" . evil-cp-backward-sexp)
;;         ("M-l" . evil-cp-end-of-defun)
;;         ("M-h" . evil-cp-beginning-of-defun)
;;         ;; ("[" . evil-cp-previous-opening)
;;         ;; ("]" . evil-cp-next-closing)
;;         ("{" . evil-cp-next-opening)
;;         ("}" . evil-cp-previous-closing)
;;         ("(" . evil-cp-backward-up-sexp)
;;         (")" . evil-cp-up-sexp))))


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

      (sam-projectile-ibuffer-by-project project-root))))

(use-package perspective
  :config
  (persp-mode))

;; (use-package persp-mode
;;   :config
;;   (define-ibuffer-filter persp-files
;;       "show ibuffer with buffers in current perspective"
;;     (:reader nil :description nil)
;;     (memq buf (persp-buffer-list)))

;;   (defun persp-ibuffer ()
;;     (interactive)
;;     (ibuffer t (format "*%s persp buffers" (persp-name (get-current-persp)))
;;              (list (cons 'persp-files ())) nil t))
;;   (setq wg-morph-on nil) ;; switch off animation
;;   (setq persp-autokill-buffer-on-remove 'kill-weak)
;;   (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))

(defun sam-switch-project-action (&optional )
  (projectile-vc)
  (delete-other-windows))

(use-package persp-projectile
  :config
  (add-hook 'projectile-after-switch-project-hook 'delete-other-windows)
  (setq projectile-switch-project-action #'projectile-vc))

(use-package visual-fill-column)

(use-package helm-recoll)

(use-package find-file-in-project)

(use-package bookmark+)

(use-package system-packages)
(use-package helm-system-packages)
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
(use-package smartparens)

(use-package smart-tabs-mode)
;; (use-package evil-cleverparens)

;; (use-package parinfer
;;   ;; :ensure t
;;   ;; :bind
;;   ;; (("M-," . parinfer-toggle-mode))
;;   :init
;;   (progn
;;     (setq parinfer-extensions
;;           '(defaults       ; should be included.
;;              pretty-parens  ; different paren styles for different modes.
;;              evil           ; If you use Evil.
;;              ;; lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
;;              paredit        ; Introduce some paredit commands.
;;              smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;;              smart-yank))   ; Yank behavior depend on mode.
;;     (setq parinfer-lighters '(" (Indent)" . " (Paren)"))))

;; (use-package parinfer)
;; (use-package smartparens)
;; (use-package paredit)
(use-package evil-cleverparens
  :init
  (setq evil-cleverparens-use-regular-insert 't))

;; (use-package lispy)
;; :config
;; (add-hook 'emacs-lisp-mode-hook #'(lambda () (evil-cleverparens-mode)))
;; (add-hook 'clojure-mode-hook #'(lambda () (evil-cleverparens-mode))))

;; (use-package evil-smartparens
;;   :config
;;   (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
;;   (general-add-hook '(emacs-lisp-mode-hook clojure-mode-hook) #'evil-smartparens-mode))

;; (use-package lispyville
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
;;   (lispyville-set-key-theme '((operators normal)
;; 							  c-w
;; 							  (prettify insert)
;; 							  (atom-movement normal visual)
;; 							  slurp/barf-lispy
;; 							  additional
;; 							  additional-insert)))

(use-package systemd)
(use-package helm-systemd)

(use-package dired-rainbow)
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook
            'all-the-icons-dired-mode
            'append))
(use-package dired-ranger)
(use-package dired-filter)
(use-package dired+
  :init
  (setq diredp-hide-details-initially-flag nil))
(use-package dired-du)
(use-package dired-subtree)
(use-package dired-filter)
(use-package dired-narrow)

(use-package highlight)
(use-package highlight-defined)

;;(use-package wuxch-dired-copy-paste)
;; (use-package ranger
;;   :config
;;   (setq ranger-override-dired-mode t)
;;   (setq ranger-cleanup-eagerly t)
;;   (setq ranger-cleanup-on-disable nil))

;; (use-package smart-tabs-mode
;;   :config
;;   (smart-tabs-insinuate 'c 'python))

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
        '(("irc.freenode.net" :channels ("#emacs" "#rcirc"))))
  (setq rcirc-authinfo
        (quote
         (("irc.freenode.net" nickserv "Overdr0ne" "(9obocohC0)")
          ("bitlbee" bitlbee "Overdr0ne" "(9obocohC0)"))))
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

;; (use-package exwm)

(use-package sx
  :config
  (evil-set-initial-state 'sx-question-list-mode 'emacs))

(use-package md4rd)

(use-package multiple-cursors)
(use-package evil-mc)

(use-package quelpa)

;; (use-package evil-vimish-fold)
;; (use-package vimish-fold)

;; (use-package queue)

(use-package gnus
  :init
  (setq user-mail-address "scmorris.dev@gmail.com"
        user-full-name "Sam")

  (setq gnus-select-method
        '(nnimap "gmail"
                 (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
                 (nnimap-server-port "imaps")
                 (nnimap-stream ssl)))

  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"))
;; (setq gnus-select-method
;;       '(nnimap "dev"
;;                (nnimap-address "imap.gmail.com")
;;                (nnimap-server-port "imaps")
;;                (nnimap-stream ssl)
;;                (nnimap-authinfo-file "~/.authinfo.gpg")
;;                (nnmail-expiry-wait immediate)))
;; :init
;; (setq gnus-select-method '(nnnil nil))
;; (setq gnus-secondary-select-methods
;;       '((nnimap "me"
;;                 (nnimap-address "imap.gmail.com")
;;                 (nnimap-server-port "imaps")
;;                 (nnimap-stream ssl)
;;                 (nnir-search-engine imap)
;;                 (nnmail-expiry-target "nnimap+home:[Gmail]/Trash")
;;                 (nnmail-expiry-wait 'immediate))
;;         (nnimap "dev"
;;                 (nnimap-address "imap.gmail.com")
;;                 (nnimap-server-port "imaps")
;;                 (nnimap-stream ssl)
;;                 (nnir-search-engine imap)
;;                 (nnmail-expiry-target "nnimap+work:[Gmail]/Trash")
;;                 (nnmail-expiry-wait 'immediate))))
;; ;; Reply to mails with matching email address
;; (setq gnus-posting-styles
;;       '((".*" ; Matches all groups of messages
;;          (address "S. C. Morris <scmorris.me@gmail.com>"))
;;         ("dev" ; Matches Gnus group called "dev"
;;          (address "S. C. Morris <scmorris.dev@gmail.com>")
;;          ;; (organization "Corp")
;;          ;; (signature-file "~/.signature-work")
;;          ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587 scmorris.dev@gmail.com")
;;          )))

(use-package all-the-icons-gnus)
(use-package nnhackernews)

(use-package wordnut)

(use-package xwwp)

;; (defun proced-settings ()
;;   (proced-toggle-auto-update))
;; (add-hook 'proced-mode-hook 'proced-settings)

(use-package calfw)
;; :config
;; (setq cfw:fchar-junction ??
;;       cfw:fchar-vertical-line ??
;;       cfw:fchar-horizontal-line ??
;;       cfw:fchar-left-junction ??
;;       cfw:fchar-right-junction ??
;;       cfw:fchar-top-junction ??
;;       cfw:fchar-top-left-corner ??
;;       cfw:fchar-top-right-corner ??))

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

;;(use-package hyperbole)

(use-package which-key
  :config
  (which-key-mode))

;; (use-package sfs
;;   :straight (sfs :type git
;; 		 :host github
;;                  :repo "Overdr0ne/sfs"
;;                  :branch "devel"
;;                  :files ("sfs.el"
;;                          "sfs-recoll.el"
;;                          "sfs-tui.el"
;;                          "sfs-tag.el"
;;                          "sfs-index.el"
;;                          "service.py")))

;;; themes
(use-package dracula-theme
  ;; :config
  ;; (load-theme 'dracula t)
  )
(use-package solarized-theme
  ;; :config
  ;; (load-theme 'solarized-light t)
  )
(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-light t))

(use-package mixed-pitch)
(use-package writeroom-mode)

(use-package multi-term)

(use-package imenu
  :config
  (setq imenu-generic-expression '(("Section"
				    "^;;;"))))

(provide '+modules)
