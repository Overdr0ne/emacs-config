;;; config/default/+bindings.el -*- lexical-binding: t; -*-

;; Fix Ascii character conflation
(setq function-key-map (delq '(kp-tab . [9]) function-key-map))
;; this is C-i
(global-set-key (kbd "C-i") (lambda () (interactive) (message "C-i")))
;; this is <tab> key
(global-set-key (kbd "<tab>") (lambda () (interactive) (message "<tab>")))

(define-key input-decode-map [?\C-\[] (kbd "<C-[>"))

;; Don't let evil-collection interfere with certain keys
(setq evil-collection-key-blacklist
      (list "<escape>"))

(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "M-p") 'term-primary-yank)))

(setq light-theme t)

(setq page-mode-map (make-sparse-keymap))

;; (bind-keys
;;  :map admin-keymap
;;  "d" #'daemons
;;  "p" #'proced
;;  "s" #'helm-systemd
;;  )
(defvar admin-keymap (make-sparse-keymap))
(general-define-key
 :keymaps 'admin-keymap
 "d" #'daemons
 "p" #'proced
 "s" #'helm-systemd)
(defvar code-keymap (make-sparse-keymap))
(general-define-key
 :keymaps 'code-keymap
 "c"   #'compile
 "e"   #'+eval/buffer-or-region
 "E"   #'+eval:replace-region
 "f"   #'+format/region-or-buffer
 "r"   #'+eval/open-repl-other-window
 "w"   #'delete-trailing-whitespace
 "x"   #'flycheck-list-errors)
(defvar go-keymap (make-sparse-keymap))
(general-define-key
 :keymaps 'go-keymap
 "."   #'(lambda () (interactive) (find-file "."))
 "h"   #'(lambda () (interactive) (find-file "~"))
 "l"   #'(lambda () (interactive) (find-file "~/src"))
 "n"   #'(lambda () (interactive) (find-file "~/notes"))
 "o"   #'(lambda () (interactive) (find-file "~/.emacs.d/overdr0ne"))
 "s"   #'(lambda () (interactive) (find-file "~/sites"))
 "t"   #'(lambda () (interactive) (find-file "~/test")))
(defvar lookup-keymap (make-sparse-keymap))
(general-define-key
 :keymaps 'lookup-keymap
 "a" #'ace-link
 "b" #'swiper
 "d" #'consult-grep-wd
 "M-d" #'rgrep-wd
 "f" #'counsel-file-jump
 "i" #'imenu
 "l" #'rgrep
 "p" #'projectile-ripgrep
 "s" #'swiper
 "w" #'wordnut-lookup-current-word
 "x" #'sx-search)
(defvar version-control-keymap (make-sparse-keymap))
(general-define-key
 :keymaps 'version-control-keymap
 "]"   #'git-gutter:next-hunk
 "["   #'git-gutter:previous-hunk
 "/"   #'magit-dispatch
 "'"   #'forge-dispatch
 "a" '(:ignore t :which-key "create")
 "ai"   #'forge-create-issue
 "ap"   #'forge-create-pullreq
 "b" '(:ignore t :which-key "branch")
 "B"   #'magit-blame-addition
 "bb"   #'magit-branch
 "bc"   #'magit-branch-and-checkout
 "c" '(:ignore t :which-key "commit")
 "cc"   #'magit-commit-create
 "cf"   #'magit-commit-fixup
 "f" '(:ignore t :which-key "file")
 "ff"   #'magit-find-file
 "fg"   #'magit-find-git-config-file
 "fc"   #'magit-show-commit
 "fi"   #'forge-visit-issue
 "fp"   #'forge-visit-pullreq
 "F"   #'magit-fetch
 "h" '(:ignore t :which-key "checkout")
 "hb"   #'magit-branch-checkout
 "hh"   #'magit-checkout
 "i"   #'magit-init
 "l" '(:ignore t :which-key "list")
 "lg"   #'+gist:list
 "lr"   #'magit-list-repositories
 "ls"   #'magit-list-submodules
 "li"   #'forge-list-issues
 "lp"   #'forge-list-pullreqs
 "ln"   #'forge-list-notifications
 "n"   #'magit-clone
 "o" '(:ignore t :which-key "browse")
 "o."   #'+vc/git-browse-region-or-line
 "or"   #'forge-browse-remote
 "oc"   #'forge-browse-commit
 "oi"   #'forge-browse-issue
 "op"   #'forge-browse-pullreq
 "oI"   #'forge-browse-issues
 "oP"   #'forge-browse-pullreqs
 "r"   #'git-gutter:revert-hunk
 "s"   #'git-gutter:stage-hunk
 "t"   #'git-timemachine-toggle
 "U"   #'magit-unstage-file
 "v"   #'magit-status
 "x"   #'magit-file-delete
 "L"   #'magit-log
 "R"   #'vc-revert
 "S"   #'magit-stage-file)

(defvar command-mode-map (make-sparse-keymap))
(general-define-key
 :keymaps 'command-mode-map
 "-"    #'ranger
 ";"    #'execute-extended-command
 ":"    #'eval-expression
 "~"    #'+popup/toggle
 ","    #'switch-to-buffer
 "."    #'find-file
 "`"    #'evil-switch-to-windows-last-buffer
 "'"    #'ivy-resume
 "/"    #'web-search
 "*" #'+default/search-project-for-symbol-at-point

 "DEL" #'+nav-flash/blink-cursor
 "RET" #'bookmark-jump
 "SPC" #'projectile-persp-switch-project
 "<tab>" #'mode-line-other-buffer

 "a" admin-keymap

 "b"   #'persp-switch-to-buffer*
 ;; "b"   #'sam-switch-to-persp-buffer
 "["   #'previous-buffer
 "]"   #'next-buffer

 "c" code-keymap

 "d" '(:ignore t :which-key "debug")
 "dd"   #'gdb
 "dm"   #'gdb-many-windows
 "ds"   #'serial-term

 "e" '(:ignore t :which-key "edit")
 "eb"   #'(lambda () (interactive) (find-file "~/.emacs.d/overdr0ne/+bindings.el"))
 "ez"   #'(lambda () (interactive) (find-file "~/.zshrc"))
 "eh"   #'(lambda () (interactive) (find-file "/etc/httpd/conf/httpd.conf"))
 "ei"   #'(lambda () (interactive) (find-file "~/.config/i3/config"))

 "f"    #'find-file

 "g" go-keymap

 "h" help-map

;;; <leader> i --- imenu
 "i" '(:ignore t :which-key "imenu")
 "if"   #'((lambda () (interactive) (sam-counsel-imenu "functions: ")) :which-key "functions")
 "ii"   #'consult-imenu
 "im"   #'(lambda () (interactive) (sam-counsel-imenu "macros: "))
 "ip"   #'(lambda () (interactive) (sam-counsel-imenu "package "))
 "is"   #'(lambda () (interactive) (sam-counsel-imenu "section: "))
 "iv"   #'(lambda () (interactive) (sam-counsel-imenu "variables: "))
 "it"   #'(lambda () (interactive) (find-file "~/test"))

 "j"    #'(lambda () (interactive) (avy-goto-word-or-subword-1))

 ;; ;;; <leader> k --- kill
 "k" #'kill-this-buffer
 ;; "k" '(:ignore t :which-key "kill")
 ;; "kb" #'kill-buffer
 ;; "kc" #'kill-buffer-and-window
 ;; "kk" #'kill-this-buffer
 ;; "kw" #'delete-window

 "l" lookup-keymap

 "m" '(:ignore t :which-key "modes")
 "mf" #'flycheck-mode
 "mF" #'toggle-frame-fullscreen
 "mg" #'evil-goggles-mode
 "mi" #'highlight-indent-guides-mode
 "mm" #'menu-bar-mode
 "mw" #'+word-wrap-mode
 "mp" #'+org-present/start
 "ms" #'flyspell-mode
 "mz" #'writeroom-mode

 "n" '(:ignore t :which-key "notes")
 "n." #'+default/browse-notes
 "n/" #'+default/org-notes-search
 "n*" #'+default/search-notes-for-symbol-at-point
 "na" #'org-agenda
 "nc" #'org-capture
 "nd" #'deft
 "nh" #'+default/org-notes-headlines
 "nl" #'org-store-link
 "nn" #'+default/find-in-notes
 "nN" #'+default/browse-notes
 "nm" #'org-tags-view
 "nv" #'org-search-view
 "nt" #'org-todo-list

 "o" '(:ignore t :which-key "open")
 "oc" #'calc
 "ob" #'browse-url-of-file
 "od" #'cfw:open-calendar-buffer
 "oe" #'eww
 "og" #'gnus
 "oP" #'+treemacs/find-file
 "oD" #'docker

 "p" '(:ignore t :which-key "project")
 "p;" #'projectile-repeat-last-command
 "p]" #'persp-next
 "p[" #'persp-prev
 "pf" #'projectile-find-file
 "p!"  #'projectile-run-shell-command-in-root
 "pa" #'projectile-add-known-project
 "pb" #'projectile-switch-to-buffer
 "pc" #'projectile-compile-project
 "pC" #'projectile-configure-project
 "pd" #'projectile-remove-known-project
 "pe" #'projectile-edit-dir-locals
 "pf" #'find-file-in-project-at-point
 "pi" #'projectile-invalidate-cache
 "pk" #'persp-kill
 "pl" #'+default/browse-project
 "pn" #'persp-switch
 "po" #'projectile-find-other-file
 "pp" #'persp-switch
 "pr" #'persp-remove-buffer
 "p/" #'(lambda () (interactive) (find-file (projectile-project-root)))
 "pR" #'projectile-run-project
 "ps" #'persp-switch
 "pT" #'projectile-test-project

 "P" '(:ignore t :which-key "packages")
 "Pe" #'counsel-package
 "Pl" #'list-packages
 "Ps" #'helm-system-packages

 "q" '(:ignore t :which-key "quit")
 "qq" #'save-buffers-kill-terminal
 "qQ" #'evil-quit-all-with-error-code

 "r" '(:ignore t :which-key "remote")
 "ru" #'ssh-deploy-upload-handler
 "rU" #'ssh-deploy-upload-handler-forced
 "rd" #'ssh-deploy-download-handler
 "rD" #'ssh-deploy-diff-handler
 "r." #'ssh-deploy-browse-remote-handler
 "r>" #'ssh-deploy-remote-changes-handler

 "s" '(:ignore t :which-key "snippets")
 "s/" #'+snippets/find-for-current-mode
 "s?" #'+snippets/find
 "sc" #'+snippets/edit
 "sf" #'+snippets/find-private
 "si" #'yas-insert-snippet
 "sn" #'+snippets/new
 "sN" #'+snippets/new-alias
 "sr" #'yas-reload-all
 "ss" #'aya-create
 "se" #'aya-expand

 "t" '(:ignore t :which-key "terminal")
 "tj" #'(lambda () (interactive) (multi-term-next))
 "tk" #'(lambda () (interactive) (multi-term-prev))
 "tt" #'(lambda () (interactive) (multi-term))
 "tr" #'+eval/open-repl-other-window
 "tR" #'+eval/open-repl-same-window
 "t;" #'(lambda () (interactive) (multi-term-dedicated-toggle))

 "u" '(:ignore t :which-key "utilities")
 "uc" #'sfs-recollect
 "ud" #'disk-usage
 "uk" #'browse-kill-ring
 "ur" #'replace-string
 ;; "uR" #'replace-query
 "us" #'sfs-research
 "uw" #'wordnut-search

 "v" #'magit-status

;;; <leader> w --- windows
 "w"    evil-window-map

;;; <leader> x --- org capture
 "x"    #'org-capture

;;; <leader> y --- hyperbole
 "y"    #'hyperbole

 "z" '(:ignore t :which-key "scratch")
 "zc" #'((lambda () (interactive) (find-file "~/scratch/c/scratch.c"))         :which-key "C")
 "zl" #'((lambda () (interactive) (find-file "~/scratch/elisp/scratch.el"))    :which-key "Elisp")
 "zp" #'((lambda () (interactive) (find-file "~/scratch/py/scratch.py"))       :which-key "Python")
 "zt" #'((lambda () (interactive) (find-file "~/scratch/text/generic.txt")) :which-key "text")
 "zz" #'((lambda () (interactive) (find-file "~/scratch/generic")) :which-key "generic"))

(defvar alter-keymap (make-sparse-keymap))
(general-define-key
 :keymaps 'alter-keymap
 "A-b" #'ibuffer)

(defvar controller-keymap (make-sparse-keymap))
(general-define-key
 :keymaps 'controller-keymap
 "<C-[>"  #'persp-prev
 "C-]"  #'persp-next
 "C-;"  #'consult-complex-command
 "C-SPC" #'tmm-menubar
 "C-b" #'persp-switch-to-buffer
 "C-c" #'sfs-recollect
 "C-f"  #'sfs-research
 "C-g" #'consult-bookmark
 "C-k" #'kill-buffer-and-window
 "C-m" #'bookmark-set
 "C-r" #'replace-string-all
 "C-t" #'(lambda () (interactive)
           (setq light-theme (not light-theme))
           (if light-theme
               (load-theme 'spacemacs-light t)
             (load-theme 'dracula t))))

(defvar metallic-keymap (make-sparse-keymap))
(general-define-key
 :keymaps 'metallic-keymap
 "M-;"  #'execute-extended-command
 "M-b" #'persp-ibuffer)

(defvar shifty-keymap (make-sparse-keymap))
(general-define-key
 :keymaps 'shifty-keymap
 "B"   #'consult-buffer)

(general-define-key
 :states 'normal
 :prefix "C-e"
 ""   nil
 "C-e" #'+eval-this-sexp
 "C-b" #'eval-buffer)

;;; Global keybindings
(general-define-key
 "C-;" #'eval-expression
 "M-;" #'iedit-mode
 "<C-m>" #'evilmi-jump-items)

(defvar hs-showing t)
(defun sam-hs-toggle-all ()
  "Toggle showing all top-level blocks"
  (interactive)
  (if hs-showing
      (progn
        (hs-hide-all)
        (setq hs-showing nil))
    (progn
      (hs-show-all)
      (setq hs-showing t))))
(general-define-key
 :states 'normal
 :keymaps '(global prog-mode-map lisp-mode-map)
 "A-j" #'evilem-motion-next-visual-line
 "A-k" #'evilem-motion-previous-visual-line
 "A-w" #'evilem-motion-forward-WORD-begin
 "A-b" #'evilem-motion-backward-WORD-begin
 "A-]" #'evilem-motion-forward-section-begin
 "A-[" #'evilem-motion-backward-section-begin

 "M-h" #'evil-digit-argument-or-evil-beginning-of-line
 "M-j" #'sam-next-line-start
 "M-k" #'sam-previous-line-start
 "M-l" #'evil-end-of-line

 "u"   #'undo

 "j"   #'evil-next-visual-line
 "k"   #'evil-previous-visual-line

 "C-j"  #'evil-scroll-line-down
 "C-k"  #'evil-scroll-line-up
 "C-h"  help-map
 "C-r" #'replace-string-all
 "n"  #'consult-line-repeat
 ;; "N"  #'isearch-repeat-backward
 "K"  #'delete-indentation
 "J"  #'evil-join
 "t"   #'(lambda () (interactive) (avy-goto-word-or-subword-1))
 "C"   #'+multiple-cursors/evil-mc-toggle-cursors
 "C-p" #'consult-yank-from-kill-ring
 "C-o" #'evil-jump-backward
 "C-i" #'evil-jump-forward
 "<tab>" #'hs-toggle-level
 "M-;" #'shelldon
 "M-:" #'shelldon-loop
 "C-M-;" #'shelldon-hist
 "C-;"   #'eval-expression
 "C-d" #'dired-jump
 "C-u" #'universal-argument
 "C-q" #'evil-quit
 "C-w" #'evil-window-map
 "C-/" #'counsel-grep-or-swiper
 "C-]" #'xref-find-definitions
 "M-]" #'xref-find-references
 ","   #'macrostep-expand
 ;; "-"   #'treemacs-add-and-display-current-project
 "-"   #'neotree-toggle
 "/"   #'consult-line
 "\\"   #'(lambda () (interactive) (avy-goto-word-or-subword-1))
 "*"   #'consult-line-symbol-at-point)

(general-define-key
 :states 'insert
 "C-a" #'beginning-of-line
 "C-e" #'end-of-line
 "C-f" #'forward-char
 "C-b" #'backward-char
 "C-n" #'next-line
 "C-p" #'previous-line
 "C-h" #'backward-delete-char-untabify
 "M-w" #'forward-word
 "M-b" #'backward-word
 ;; Smarter newlines
 [remap newline] #'newline-and-indent  ; auto-indent on newline
 ;; "C-j"           #'+default/newline    ; default behavior
 "C-v" 'yank
 "<mouse-2>" 'evil-paste-after)

(general-define-key
 :states '(normal visual)
 :keymaps '(global prog-mode-map)
 "j"   #'evil-next-visual-line
 "k"   #'evil-previous-visual-line
 "f"  #'evil-snipe-f
 "F"  #'evil-snipe-F
 "w"  #'forward-to-word
 "e"  #'forward-word
 "b"  #'backward-word
 "z=" #'flyspell-correct-word-generic
 ";"   #'execute-extended-command)

(defhydra hydra-undo-tree (:color yellow
                                  :hint nil)
  "
 _u_: undo  _r_: redo _s_: save _l_: load "
  ("u"   undo-tree-undo)
  ("r"   undo-tree-redo)
  ("s"   undo-tree-save-history)
  ("l"   undo-tree-load-history)
  ("v"   undo-tree-visualize "visualize" :color blue)
  ("q"   nil "quit" :color blue))
;; (general-define-key
;;  :states 'normal
;;  :keymaps '(global evil-cleverparens-mode-map)
;;  (kbd "u") 'undo
;;  ;; (kbd "U") 'hydra-undo-tree/undo-tree-redo
;;  ;; (kbd "u") 'hydra-undo-tree/undo-tree-undo
;;  )

(general-define-key
 :states 'visual
 :keymaps 'prog-mode-map
 ;; "d"   #'evil-delete
 "v"   #'er/expand-region
 "C-j" #'evil-join
 "A-s" #'shell-command-on-region
 "x"   #'+multiple-cursors/evil-mc-make-cursor-here
 "@"   #'+evil:apply-macro
 "<"   #'evil-shift-left
 ">"   #'evil-shift-right)

(general-define-key
 :states 'motion
 "]m"  #'+evil/next-beginning-of-method
 "[m"  #'+evil/previous-beginning-of-method
 "]M"  #'+evil/next-end-of-method
 "[M"  #'+evil/previous-end-of-method
 "]#"  #'+evil/next-preproc-directive
 "[#"  #'+evil/previous-preproc-directive
 "]*"  #'+evil/next-comment
 "[*"  #'+evil/previous-comment
 "]\\" #'+evil/next-comment
 "[\\" #'+evil/previous-comment)

(define-key evil-cleverparens-mode-map (kbd "<normal-state> [") nil)
(define-key evil-cleverparens-mode-map (kbd "<normal-state> ]") nil)
(general-define-key
 :states '(normal motion)
 :keymaps 'evil-cleverparens-mode-map
 "]]" 'evil-forward-section-begin
 "][" 'evil-forward-section-end
 "[[" 'evil-backward-section-begin
 "[]" 'evil-backward-section-end
 "[(" 'evil-previous-open-paren
 "])" 'evil-next-close-paren
 "[{" 'evil-previous-open-brace
 "]}" 'evil-next-close-brace
 "]s" 'evil-next-flyspell-error
 "[s" 'evil-prev-flyspell-error)

(general-define-key :states '(normal) "C-=" #'sam-indent-all)
(general-define-key :states '(visual) "=" #'indent-region)
(general-define-key
 :states 'normal
 :prefix "="
 ""  nil
 "=" #'evil-indent-line
 "s" #'indent-sexp
 "a" #'sam-indent-all
 "g" #'indent-rigidly)

;; ported from vim-unimpaired
(general-define-key
 :states 'normal
 "]b"    #'next-buffer
 "[b"    #'previous-buffer
 "]f"    #'+evil/next-file
 "[f"    #'+evil/previous-file)

(general-define-key
 :states 'motion
 "]u" #'+evil:url-encode
 "[u" #'+evil:url-decode
 "]y" #'+evil:c-string-encode
 "[y" #'+evil:c-string-decode)

;; evil surround
(general-define-key
 :states 'visual
 "S" #'evil-surround-region)

(general-define-key
 :states 'operator
 "s" #'evil-surround-edit
 "S" #'evil-Surround-edit)

;;; Evil override keybindings
(evil-define-key 'normal magit-mode-map (kbd "C-d") 'dired-jump)

;;; Space keybindings
(general-define-key
 :keymaps '(global dired-mode-map magit-mode-map Info-mode-map doc-view-mode-map help-mode-map Man-mode-map grep-mode-map)
 :states '(normal visual)
 "C-SPC" controller-keymap
 "M-SPC" metallic-keymap
 "S-SPC" shifty-keymap
 "A-SPC" alter-keymap
 "SPC" command-mode-map)

;;; Module keybinds
(general-define-key
 :states 'normal
 :keymaps 'markdown-mode-map
 "C-d" #'dired-jump)
(general-define-key
 :keymaps 'treemacs-mode-map
 :states 'normal
 "-" #'treemacs-quit)
(general-define-key
 :states 'normal
 :keymaps 'view-mode-map
 "e" #'view-mode)

(general-define-key
 :states 'normal
 :keymaps 'outline-mode-map
 "C-;" #'eval-expression)

(general-define-key
 :states 'normal
 :keymaps '(help-mode-map helpful-mode-map)
 "M-b" #'backward-button
 "M-w" #'forward-button)

(general-define-key
 :states 'normal
 :keymaps '(Info-mode-map)
 "C-n" #'Info-forward-node
 "C-p" #'Info-backward-node
 "C-h" #'Info-history-back
 "C-j" #'evil-window-down
 "C-k" #'evil-window-up
 "C-l" #'Info-history-forward
 "M-h" #'Info-prev
 "M-k" #'Info-up
 "M-l" #'Info-next
 "M-w" #'Info-next-reference
 "M-b" #'Info-prev-reference
 )

;;; :completion
(general-define-key
 :keymaps 'prog-mode-map
 :states 'insert
 "C-j" #'completion-at-point)
(general-define-key
 :keymaps 'corfu-map
 :state 'insert
 "C-j" #'corfu-next
 "C-k" #'corfu-previous
 "C-l" #'corfu-complete)

(define-key dired-mode-map (kbd "<normal-state> ;") nil)
(general-define-key
 :keymaps 'dired-mode-map
 :state 'normal
 :prefix "y"
 ""  nil
 ";" #'execute-extended-command
 "d" #'(lambda () (interactive) (kill-new (pwd)))
 "y" #'dired-ranger-copy
 "m" #'dired-ranger-move
 "p" #'dired-ranger-paste)
(general-define-key
 :keymaps 'dired-mode-map
 :states 'normal
 ""      nil
 "C-<return>" #'shelldon
 "C-/" #'dired-narrow
 "C-l" #'dired-do-symlink
 "h"   #'dired-up-directory
 "l"   #'dired-find-file)
;; (define-key dired-mode-map (kbd "<normal-state> y y") 'dired-ranger-copy)
;; (define-key dired-mode-map (kbd "y m") 'dired-ranger-move)
;; (evil-define-key 'normal
;;   dired-mode-map
;;   "yy"  #'dired-ranger-copy
;;   "ym"  #'dired-ranger-copy
;;   "yp"  #'dired-ranger-copy
;;   "\C-l" #'dired-do-symlink
;;   "h"   #'dired-up-directory
;;   "l"   #'dired-find-file)

(general-define-key
 :keymaps '(smerge-mode-map)
 :states  '(normal)
 "<return>" #'smerge-keep-current)

(evil-define-key 'normal
  sclang-mode-map
  "\C-e" #'+sclang-eval-this-expression)

;;; :ui
(general-define-key
 :keymaps '(evil-window-map)
 ;; Navigation
 "C-h" #'evil-window-left
 "C-j" #'evil-window-down
 "C-k" #'evil-window-up
 "C-l" #'evil-window-right
 "C-d" #'delete-window
 "C-q" #'quit-window
 "C-t" #'window-toggle-side-windows
 "C-w" #'other-window
 "C-n" #'evil-window-vnew
 ;; Swapping windows
 "C-M-h" #'evil-window-move-far-left
 "C-M-j" #'evil-window-move-very-bottom
 "C-M-k" #'evil-window-move-very-top
 "C-M-l" #'evil-window-move-far-right
 "C-a"   #'ace-swap-window
 "u"     #'winner-undo
 "C-u"   #'winner-undo
 "C-r"   #'winner-redo
 "o"     #'delete-other-windows
 ;; Delete window
 "C-C"     #'ace-delete-window)

;;; :tools

(general-define-key
 :keymaps 'evil-magit
 ;; fix conflicts with private bindings
 :map '(gnus-summary-mode gnus-article-mode)
 "SPC" nil)

(general-define-key
 :keymaps 'evil-magit
 ;; fix conflicts with private bindings
 :map '(magit-mode-map magit-status-mode-map magit-revision-mode-map)
 "k"   'magit-checkout
 "C-d" 'dired-jump
 "C-j" nil
 "C-k" nil)
;; (map! (:when (featurep! :tools eval)
;;         :g  "M-r" #'+eval/buffer
;;         :nv "gr"  #'+eval:region
;;         :n  "gR"  #'+eval/buffer
;;         :v  "gR"  #'+eval:replace-region)

;;       ;; (:when (featurep! :tools spell)
;;         ;; Keybinds that have no Emacs+evil analogues (i.e. don't exist):
;;         ;;   zq - mark word at point as good word
;;         ;;   zw - mark word at point as bad
;;         ;;   zu{q,w} - undo last marking
;;         ;; Keybinds that evil define:
;;         ;;   z= - correct flyspell word at point
;;         ;;   ]s - jump to previous spelling error
;;         ;;   [s - jump to next spelling error
;;         ;; (:map flyspell-mouse-map
;;         ;;       "RET"     #'flyspell-correct-word-generic
;;         ;;       [return]  #'flyspell-correct-word-generic
;;         ;;       [mouse-1] #'flyspell-correct-word-generic))

;;       ;;      (:when (featurep! :checkers syntax)
;;       ;;        (:after flycheck
;;       ;;          :map flycheck-error-list-mode-map
;;       ;;          :n "C-n"    #'flycheck-error-list-next-error
;;       ;;          :n "C-p"    #'flycheck-error-list-previous-error
;;       ;;          :n "j"      #'flycheck-error-list-next-error
;;       ;;          :n "k"      #'flycheck-error-list-previous-error
;;       ;;          :n "RET"    #'flycheck-error-list-goto-error
;;       ;;          :n [return] #'flycheck-error-list-goto-error))

;;       ;; (:when (featurep! :tools gist)
;;       ;;   :after gist
;;       ;;   :map gist-list-menu-mode-map
;;       ;;   :n "go"  #'gist-browse-current-url
;;       ;;   :n "gr"  #'gist-list-reload
;;       ;;   :n "c"   #'gist-add-buffer
;;       ;;   :n "d"   #'gist-kill-current
;;       ;;   :n "e"   #'gist-edit-current-description
;;       ;;   :n "f"   #'gist-fork
;;       ;;   :n "q"   #'kill-current-buffer
;;       ;;   :n "s"   #'gist-star
;;       ;;   :n "S"   #'gist-unstar
;;       ;;   :n "y"   #'gist-print-current-url)

;;       (:when (featurep! :tools lookup)
;;         :nv "gk"  #'+lookup/documentation
;;         :nv "gd" #'+lookup/definition
;;         :nv "gD" #'+lookup/references
;;         :nv "gf" #'+lookup/file)

;;       (:when (featurep! :tools magit)
;;         (:after evil-magit
;;                 ;; fix conflicts with private bindings
;;                 :map (magit-mode-map magit-status-mode-map magit-revision-mode-map)
;;                 "k"   'magit-checkout
;;                 "C-d" 'dired-jump
;;                 "C-j" nil
;;                 "C-k" nil)
;;         (:map transient-map
;;               "q" #'transient-quit-one)))

;; (:map package-menu-mode-map
;;  )

;;; :lang
;; (evil-define-key '(normal visual) evil-cleverparens-mode-map
;;   (kbd "M-w") #'sp-next-sexp
;;   (kbd "M-e") #'sp-forward-sexp
;;   (kbd "M-b") #'sp-backward-sexp)
(general-define-key
 :states '(normal visual)
 :keymaps 'evil-cleverparens-mode-map
 "("   #'sp-backward-up-sexp
 ")"   #'sp-up-sexp
 "M-w" #'sp-next-sexp
 "M-e" #'sp-forward-sexp
 "M-b" #'sp-backward-sexp)

(general-define-key
 :states '(normal insert)
 :keymaps 'evil-cleverparens-mode-map
 "M-i" #'evil-cp-insert-at-beginning-of-form
 "M-a" #'evil-cp-insert-at-end-of-form)

(general-define-key
 :states 'normal
 :keymaps 'evil-cleverparens-mode-map
 ;; (kbd "M-<tab>") #'parinfer-toggle-mode
 "M-p" #'evil-cp-copy-paste-form
 "M-y" #'evil-cp-yank-sexp
 "M-d" #'evil-cp-delete-sexp
 "M-h" #'evil-digit-argument-or-evil-beginning-of-line
 "M-j" #'sam-next-line-start
 "M-k" #'sam-previous-line-start
 "M-l" #'evil-end-of-line
 "H"   #'evil-window-top
 "L"   #'evil-window-bottom
 "w"   #'forward-to-word
 "e"   #'forward-word
 "b"   #'backward-word
 "M-9" #'paredit-wrap-sexp
 "M-0" #'sp-unwrap-sexp
 "S"   #'evil-snipe-S
 "s"   #'evil-snipe-s)

(general-define-key
 :states 'normal
 :keymaps 'sexpy-mode-map
 "C-M-w"   #'sam-drag-sexp-forward
 "C-M-b"   #'sam-drag-sexp-backward)

(general-define-key
 :states 'normal
 :keymaps 'fundamental-mode-map
 "C-d" #'dired-jump)

(general-define-key
 :states 'insert
 :keymaps 'prog-mode-map
 "<backtab>" #'indent-rigidly-left
 "<tab>"     #'indent-relative
 "M-<tab>"   #'indent-relative-below)
(general-define-key
 :states '(normal visual)
 :keymaps 'prog-mode-map
 "RET" #'evil-commentary-line)

(general-define-key
 :states 'normal
 :keymaps 'evil-org-mode-map
 "<return>" #'org-toggle-narrow-to-subtree
 "t"        #'org-set-tags-command
 "pp"       #'evil-paste-after
 "pl"       #'org-insert-link)
(general-define-key
 :states 'insert
 :keymaps 'evil-org-mode-map
 "<tab>"     #'org-do-demote
 "<backtab>" #'org-do-promote)

(general-define-key
 :keymaps 'global
 :states 'insert
 "M-SPC" metallic-keymap)

(define-key input-decode-map [?\C-m] [C-m])

(general-define-key
 :keymaps 'evil-ex-completion-map
 ;; "C-a" #'move-beginning-of-line
 ;; "C-b" #'backward-word
 "C-a"           #'beginning-of-line
 "C-e"           #'end-of-line
 "C-f"           #'forward-char
 "C-b"           #'backward-char
 "M-w"           #'forward-word
 "M-b"           #'backward-word
 "C-s" #'counsel-minibuffer-history)
(defvar +default-minibuffer-maps
  `(minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-isearch-map
    ;; selectrum-minibuffer-map
    read-expression-map
    ivy-switch-buffer-map)
  "A list of all the keymaps used for the minibuffer.")
(general-define-key
 :keymaps +default-minibuffer-maps
 [escape] #'abort-recursive-edit
 "C-M-h"  help-map
 "C-v"    #'yank
 "C-a"    #'beginning-of-line
 "C-e"    #'end-of-line
 "C-f"    #'forward-char
 "C-b"    #'backward-char
 "C-h"    #'backward-delete-char-untabify
 "C-l"    #'completion-at-point
 "M-w"    #'forward-word
 "M-b"    #'backward-word
 "C-r"    #'evil-paste-from-register
 "C-j" #'next-line-or-history-element
 "C-k" #'previous-line-or-history-element
 "C-r" #'previous-matching-history-element
 "M-n" #'next-line-or-history-element
 "M-p" #'previous-line-or-history-element)

(defvar +minibuffer-completion-maps
  '(ivy-minibuffer-map selectrum-minibuffer-map minibuffer-local-completion-map minibuffer-local-must-match-map)
  "A list of all the keymaps used for minibuffer completion.")
(general-define-key
 :keymaps +minibuffer-completion-maps
 "<tab>" #'minibuffer-complete
 "C-v" #'yank
 "C-a" #'beginning-of-line
 "C-e" #'end-of-line
 "C-f" #'forward-char
 "C-b" #'backward-char
 "C-g" #'exit-minibuffer
 "C-h" #'backward-delete-char-untabify
 "C-l" #'ivy-partial-or-done
 "M-w" #'forward-word
 "M-b" #'backward-word
 "M-h" #'backward-kill-word
 "C-r" #'previous-matching-history-element
 "C-j" #'next-line
 "C-k" #'previous-line)
(general-define-key
 :keymaps 'selectrum-minibuffer-map
 "<tab>" #'selectrum-insert-current-candidate
 "C-l"   #'selectrum-insert-current-candidate)

(general-define-key
 :keymaps '(help-map)
 ;; new keybinds
 "'"    #'describe-char
 ;; Unbind `help-for-help'. Conflicts with which-key's help command for the
 ;; <leader> h prefix. It's already on ? and F1 anyway.
 "C-h"  nil
 "a"    #'consult-apropos
 "b"    #'counsel-descbinds
 "C-b"  #'counsel-descbinds
 "c"    #'helpful-command
 "C-c"  #'describe-coding-system
 "f"    #'helpful-callable
 "F"    #'counsel-describe-face
 "h"    #'helpful-at-point
 "k"    #'helpful-key
 "C-k"  #'which-key-show-top-level
 "C-l"  #'describe-language-environment
 "L"    #'global-command-log-mode
 "C-m"  #'info-emacs-manual
 ;; replaces `finder-by-keyword'
 "o"    #'ace-link-help
 "p"    #'describe-package
 ;; replaces `describe-package' b/c redundant w/ `doom/describe-package'
 "P"    #'find-library
 "v"    #'helpful-variable
 "V"    #'set-variable)

(defhydra evil-window-hydra (:color yellow :hint nil)
  "
 _h_: window-left  _j_: window-down _k_: window-up _l_: window-right "
  ("h"   evil-window-left)
  ("j"   evil-window-down)
  ("k"   evil-window-up)
  ("l"   evil-window-right)
  ("q"   nil "quit" :color blue))

(general-define-key
 :keymaps 'evil-window-map
 "h" #'evil-window-hydra/evil-window-left
 "k" #'evil-window-hydra/evil-window-up
 "j" #'evil-window-hydra/evil-window-down
 "l" #'evil-window-hydra/evil-window-right)

(general-define-key
 :states '(insert visual)
 :keymaps '(global prog-mode-map)
 "<C-[>" #'evil-force-normal-state)
