;;; +bindings.el --- personal bindings configuration    -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Sam

;; Author: Overdr0ne
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

;; Set up my bindings.

;;; Code:

(require '+modules)
(require '+sets)

;; Fix Ascii character conflation
;;(setq function-key-map (delq '(kp-tab . [9]) function-key-map))

;;; space maps
;; (defvar admin-keymap (make-sparse-keymap))
;; (general-define-key
;;  :keymaps 'admin-keymap
;;  "d" #'daemons
;;  "p" #'proced
;;  "s" #'helm-systemd)
;; (defvar code-keymap (make-sparse-keymap))
;; (general-define-key
;;  :keymaps 'code-keymap
;;  "c"   #'multi-compile-run
;;  "r"   #'+eval/open-repl-other-window
;;  "w"   #'delete-trailing-whitespace
;;  "x"   #'flycheck-list-errors)
(defvar +file-keymap (make-sparse-keymap))
(skey-define-keys
 '(+file-keymap)
 `(
   ("."   ffap)
   ("f"   find-file)
   ("c"   copy-file)
   ("d"   delete-file)
   ("i"   insert-file)
   ("l"   load-file)
   ("m"   make-directory)
   ("r"   rename-file)
   ))

(defvar +go-keymap (make-sparse-keymap))
(skey-define-keys
 '(+go-keymap)
 `(("."   (lambda () (interactive) (find-file ".")))
   ("h"   (lambda () (interactive) (find-file "~")))
   ("s"   (lambda () (interactive) (find-file "~/src")))
   ("n"   (lambda () (interactive) (find-file "~/notes")))
   ("o"   (lambda () (interactive) (find-file "~/.emacs.d/overdr0ne")))
   ("t"   (lambda () (interactive) (find-file "~/test")))
   ("w"   (lambda () (interactive) (find-file "~/workspaces")))))
(defvar +help-map (make-composed-keymap help-map))
(skey-define-keys
 '(+help-map)
 `(("'"    #'describe-char)
   ("C-h"  nil)
   ("a"    consult-apropos)
   ("c"    helpful-command)
   ("C-c"  describe-coding-system)
   ("f"    helpful-callable)
   ("F"    describe-face)
   ("h"    helpful-at-point)
   ("H"    sam-lookup-symbol-at-point)
   ("k"    helpful-key)
   ("C-k"  which-key-show-top-level)
   ("C-l"  describe-language-environment)
   ("L"    global-command-log-mode)
   ("C-m"  info-emacs-manual)
   ("o"    ace-link-help)
   ("p"    describe-package)
   ("P"    find-library)
   ("v"    helpful-variable)
   ("V"    set-variable)
   ))
(defvar +lookup-keymap (make-sparse-keymap))
(skey-define-keys
 '(+lookup-keymap)
 `(("a" ace-link)
   ("d" sam-ripgrep-wd)
   ("D" sam-ripgrep-dir)
   ("M-d" rgrep-wd)
   ("f" sam-find-file-here)
   ("i" imenu)
   ("l" rgrep)
   ("p" consult-ripgrep)
   ("w" wordnut-lookup-current-word)
   ("x" sx-search)))

;;; windows
(defvar +window-map (make-sparse-keymap))
(skey-define-keys
 '(+window-map)

 `(("C-h" windmove-left)
   ("C-M-h" sam-delete-left-side-window)
   ("C-j" windmove-down)
   ("C-k" windmove-up)
   ("C-l" windmove-right)
   ("C-M-l" sam-delete-right-side-window)
   ("C-o" delete-other-windows)
   ("C-p" window-swap-states)
   ("C-d" delete-window)
   ("C-q" quit-window)
   ("C-s" window-swap-states)
   ("C-t" window-toggle-side-windows)
   ("C-v" split-window-right)
   ("C-n" split-window-vertically)
   ("C-w" other-window)

   ("M-h" windmove-display-left)
   ("M-j" windmove-display-down)
   ("M-k" windmove-display-up)
   ("M-l" windmove-display-right)
   ("M-c" windmove-display-same-window)

   ("<C-m>" sam-ace-move-window)
   ("C-u"   winner-undo)
   ("C-r"   winner-redo)
   ))

;; (defvar version-control-keymap (make-sparse-keymap))
;; (general-define-key
;;  :keymaps 'version-control-keymap
;;  "]"   #'git-gutter:next-hunk
;;  "["   #'git-gutter:previous-hunk
;;  "/"   #'magit-dispatch
;;  "'"   #'forge-dispatch
;;  "a" '(:ignore t :which-key "create")
;;  "ai"  #'forge-create-issue
;;  "ap"  #'forge-create-pullreq
;;  "b" '(:ignore t :which-key "branch")
;;  "B"   #'magit-blame-addition
;;  "bb"  #'magit-branch
;;  "bc"  #'magit-branch-and-checkout
;;  "c" '(:ignore t :which-key "commit")
;;  "cc"  #'magit-commit-create
;;  "cf"  #'magit-commit-fixup
;;  "f" '(:ignore t :which-key "file")
;;  "ff"  #'magit-find-file
;;  "fg"  #'magit-find-git-config-file
;;  "fc"  #'magit-show-commit
;;  "fi"  #'forge-visit-issue
;;  "fp"  #'forge-visit-pullreq
;;  "F"   #'magit-fetch
;;  "h" '(:ignore t :which-key "checkout")
;;  "hb"  #'magit-branch-checkout
;;  "hh"  #'magit-checkout
;;  "i"   #'magit-init
;;  "l" '(:ignore t :which-key "list")
;;  "lr"  #'magit-list-repositories
;;  "ls"  #'magit-list-submodules
;;  "li"  #'forge-list-issues
;;  "lp"  #'forge-list-pullreqs
;;  "ln"  #'forge-list-notifications
;;  "n"   #'magit-clone
;;  "o" '(:ignore t :which-key "browse")
;;  "or"  #'forge-browse-remote
;;  "oc"  #'forge-browse-commit
;;  "oi"  #'forge-browse-issue
;;  "op"  #'forge-browse-pullreq
;;  "oI"  #'forge-browse-issues
;;  "oP"  #'forge-browse-pullreqs
;;  "r"   #'git-gutter:revert-hunk
;;  "s"   #'git-gutter:stage-hunk
;;  "t"   #'git-timemachine-toggle
;;  "U"   #'magit-unstage-file
;;  "v"   #'magit-status
;;  "x"   #'magit-file-delete
;;  "L"   #'magit-log
;;  "R"   #'vc-revert
;;  "S"   #'magit-stage-file)

(skey-define-keys
 '(project-prefix-map)
 '(("f" project-find-file)
   ("p" sam-project-persp-switch-project)
   ("v" magit-status)))

(defvar +command-mode-map (make-sparse-keymap))
(skey-define-keys
 '(+command-mode-map)
 `(
   (";"    execute-extended-command)
   (":"    eval-expression)
   (","    switch-to-buffer)
   ("."    find-file)
   ("RET" bookmark-jump)
   ("SPC" persp-switch)
   ("<tab>" mode-line-other-buffer)

   ("b"   sam-bitbake)
   ("B"   revert-buffer)
   ;; "b"   sam-switch-to-persp-buffer)
   ("["   previous-buffer)
   ("]"   next-buffer)

   ("dd"   gdb)
   ("dm"   gdb-many-windows)
   ("ds"   sam-serial-term)

   ("eb"   (lambda () (interactive) (find-file "~/.bashrc")))
   ("ez"   (lambda () (interactive) (find-file "~/.zshrc")))
   ("eh"   (lambda () (interactive) (find-file "/etc/httpd/conf/httpd.conf")))
   ("ei"   (lambda () (interactive) (find-file "~/.config/i3/config")))

   ("f" ,+file-keymap)
   ("F"    sam-find-root)

   ("g" ,+go-keymap)

   ("h" ,+help-map)

   ("ii"   consult-imenu)
   ("if"   consult-imenu-functions)
   ("im"   consult-imenu-macros)
   ("ip"   consult-imenu-packages)
   ("it"   consult-imenu-types)
   ("iv"   consult-imenu-variables)

   ("j"    avy-goto-word-or-subword-1)

   ("k" kill-current-buffer)

   ("l" ,+lookup-keymap)

   ("mf" flycheck-mode)
   ("mF" toggle-frame-fullscreen)
   ("mg" evil-goggles-mode)
   ("mh" hl-line-mode)
   ("mm" menu-bar-mode)
   ("ms" flyspell-mode)
   ("mv" visual-line-mode)
   ("mz" writeroom-mode)

   ("na" org-agenda)
   ("nc" org-capture)
   ("nd" deft)
   ("nl" org-store-link)
   ("nn" (lambda () (interactive) (find-file "~/notes/")))
   ("nm" org-tags-view)
   ("nv" org-search-view)
   ("nt" org-todo-list)

   ("oc" calc)
   ("ob" browse-url-of-file)
   ("od" docker)
   ("oD" cfw:open-calendar-buffer)
   ("oe" eww)
   ("og" gnus)

   ("p" ,project-prefix-map)

   ("Pl" list-packages)
   ("Ps" helm-system-packages)

   ("qq" save-buffers-kill-terminal)

   ("si" yas-insert-snippet)
   ("sr" yas-reload-all)
   ("ss" aya-create)
   ("se" aya-expand)

   ("t"   evim-term)
   ("T"   evim-term-side)

   ("uc" sfs-recollect)
   ("ud" disk-usage)
   ("uk" browse-kill-ring)
   ("ur" replace-string)
   ("us" sfs-research)
   ("uw" wordnut-search)

   ("v" magit-status)

   ("x"    +persp-pop-to-scratch)

   ("y"    org-capture)

   ("zc" ((lambda () (interactive) (find-file "~/scratch/c/scratch.c")) :which-key "C"))
   ("zl" ((lambda () (interactive) (find-file "~/scratch/elisp/scratch.el")) :which-key "Elisp"))
   ("zp" ((lambda () (interactive) (find-file "~/scratch/py/scratch.py")) :which-key "Python"))
   ("zt" ((lambda () (interactive) (find-file "~/scratch/text/generic.txt")) :which-key "text"))
   ("zz" ((lambda () (interactive) (find-file "~/scratch/generic")) :which-key "generic"))))

(skey-define-keys
 '(evim-normal-mode-map)
 `(("SPC" ,+command-mode-map)
   ("-" evim-join)))

(defvar +alter-keymap (make-sparse-keymap))
(skey-define-keys
 '(+alter-keymap)
 `(("A-b" ibuffer)))

(defvar +controller-keymap (make-sparse-keymap))
(skey-define-keys
 '(+controller-keymap)
 `(
   ("<C-[>"  persp-prev)
   ("C-]"  persp-next)
   ("C-;"  consult-complex-command)
   ("C-SPC" tmm-menubar)
   ("C-b" persp-switch-to-buffer)
   ;; ("C-c" sfs-recollect)
   ;; ("C-f"  sfs-research)
   ;; ("C-g" consult-bookmark)
   ("C-k" kill-buffer-and-window)
   ("<C-m>" bookmark-set)
   ("C-t" sam-toggle-theme)))

(defvar slink-keymap (make-sparse-keymap))
(skey-define-keys
 '(slink-keymap)
 `(
   ("<C-return>" slink-load)
   ("C-d" slink-delete)
   ("C-f" slink-save-file)
   ("C-g" slink-get-url-at-point)
   ("C-s" slink-save)
   ("C-e" slink-edit-label)))

(defvar +metallic-keymap (make-sparse-keymap))
(skey-define-keys
 '(+metallic-keymap)
 `(("M-;" execute-extended-command)
   ("M-b" persp-ibuffer)
   ("M-g" consult-find)
   ("M-f" consult-recent-file)))

(defvar +shifty-keymap (make-sparse-keymap))
(skey-define-keys
 '(+shifty-keymap)
 `(("B" consult-buffer)
   ("F" sam-sudo-find-file)
   ("f" sam-sudo-find-root)
   ("R" sam-sudo-find-root)))

(require 'man)
(require 'woman)
(require 'conf-mode)
(require 'grep)
(require 'dired)

(defvar root-maps
  '(term-mode-map
    Man-mode-map
    woman-mode-map
    prog-mode-map
    compilation-mode-map
    lisp-mode-map
    outline-mode-map
    help-mode-map
    helpful-mode-map
    Custom-mode-map
    text-mode-map
    shelldon-mode-map
    shell-mode-map
    conf-mode-map))

(skey-define-keys
 '(evim-insert-mode-map)
 `(
   ("SPC" self-insert-command)))

(defvar +buffer-keymap (make-sparse-keymap))
(skey-define-keys
 '(+buffer-keymap)
 `(
   ("C-a" sam-switch-to-persp-buffer)
   ("C-b" persp-switch-to-buffer*)
   ("C-c" kill-current-buffer)
   ("C-d" sam-switch-to-dir)
   ;; ("C-h" buf-move-left)
   ;; ("C-j" buf-move-down)
   ;; ("C-k" buf-move-up)
   ;; ("C-l" buf-move-right)
   ("C-n" rename-buffer)
   ("C-k" persp-remove-buffer)
   ("C-r" revert-buffer)))

(defvar +tab-keymap (make-sparse-keymap))
(skey-define-keys
 '(+tab-keymap)
 `(
   ("C-h" tab-bar-switch-to-prev-tab)
   ("C-l" tab-bar-switch-to-next-tab)

   ("C-t" tab-bar-switch-to-tab)
   ))

(defvar +kill-keymap (make-sparse-keymap))
(skey-define-keys
 '(+kill-keymap)
 `(
   ("M-b" kill-buffer)
   ("M-c" kill-current-buffer)
   ("M-r" kill-buffer-and-window)))

(defvar +eval-keymap (make-sparse-keymap))
(skey-define-keys
 '(+eval-keymap)
 `(
   ("C-e" sam-eval-this-sexp)
   ("C-b" eval-buffer)))

(require 'debug)

;;; holy motions
(skey-define-keys
 '(evim-normal-mode-map evim-visual-mode-map)
 `(
   ("C-M-j" holymotion-next-visual-line)
   ("C-M-k" holymotion-previous-visual-line)

   ("C-M-w" holymotion-forward-to-word)
   ("C-M-e" holymotion-forward-word)
   ("C-M-b" holymotion-backward-word)
   ))

(skey-define-keys
 '(evim-insert-mode-map)
 `(
   ("C-M-j" holymotion-next-visual-line)
   ("C-M-k" holymotion-previous-visual-line)

   ("C-M-f" holymotion-forward-to-word)
   ("C-M-e" holymotion-forward-word)
   ("C-M-b" holymotion-backward-word)
   ))

(skey-define-keys
 '(evim-insert-mode-map)
 `(
   ("`" (lambda () (interactive) (insert "`")))
   ("C-M-h" sam-kill-backward-line)
   ("<C-m>" sam-match)
   ("C-v" consult-yank-from-kill-ring)))

(skey-define-keys
 '(evim-visual-mode-map)
 `(
   ("<return>" sam-pushb-or-embark)
   ("<C-m>" sam-match)
   ("r" anzu-query-replace-regexp)))

(skey-define-keys
 '(evim-normal-mode-map)
 `(
   ;;  "s"   nil
   ;;  "<M-mouse-3>" #'sam-toggle-cursor
   ;;  "<mouse-2>" #'sam-helpful-click

   ("<backspace>" switch-to-prev-buffer)
   ("<S-backspace>" switch-to-next-buffer)

   ("<return>" sam-pushb-or-embark)
   ("<C-return>" ,slink-keymap)

   ;;  "<tab>" #'hs-toggle-block

   ("] SPC" sam-insert-line-below)
   ("C-]" xref-find-definitions)
   ("<M-]>" xref-find-references)

   ("[ SPC" sam-insert-line-above)

   ("C-'" consult-mark)
   ;;  ","   #'macrostep-expand
   ("'"   jump-to-register)
   ("-"   evim-join)

   ("/"   sam-comment-line)
   ("C-/" comment-dwim)
   ;;  "\\"   #'avy-goto-word-or-subword-1
   ("\""  consult-register)
   ("*" consult-line-symbol-at-point)
 
   ("+" goto-line)

   (";" execute-extended-command)
   ("C-;" eval-expression)
   ;;  "M-;" #'shelldon
   ;;  "M-:" #'shelldon-loop
   ;;  "M-C-;" #'shelldon-output-history

   ("M-A" sam-insert-at-end-of-form)

   ("C-b" ,+buffer-keymap)

   ("C-d" dired-jump)

   ("C-e" ,+eval-keymap)

   ("f" ,+file-keymap)
   ("C-f" consult-line)
   ("C-M-f" consult-line-repeat)
   ;;  "C-M-f" #'sfs-research

   ;;  "C-h"  help-map
   ;;  "M-h" #'back-to-indentation

   ("C-i" gumshoe-peruse-in-buffer)
   ;; ("M-i" gumshoe-peruse-in-persp)
   ("C-M-i" gumshoe-peruse-globally)
   ;;  "C-i" #'gumshoe-buf-backtrack-forward
   ;;  "M-i" #'gumshoe-persp-backtrack-forward
   ;;  "C-M-i" #'gumshoe-backtrack-forward
   ;;  "C-S-i" #'gumshoe-peruse-in-buffer
   ;;  "M-S-i" #'gumshoe-peruse-in-persp
   ;;  "C-M-S-i" #'gumshoe-peruse-globally

   ("j" next-line)
   ("C-j" sam-scroll-page-up)
   ("J"  sam-scroll-up-command)
   ;; ("M-j" sam-next-line-start)
   ("M-j" scroll-up-line)
   ;; ("C-j" scroll-up-line)
   ;; ("C-j" sam-scroll-page-up)
   ;; ("C-M-j" sam-scroll-page-up)
   ;; ("C-M-j" holymotion-next-visual-line)

   ("k" previous-line)
   ("C-k" sam-scroll-page-down)
   ("K"  sam-scroll-down-command)
   ;; ("M-k" sam-previous-line-start)
   ("M-k" scroll-down-line)
   ;; ("C-k" scroll-down-line)
   ;; ("C-k" sam-scroll-page-down)
   ;; ("M-k" sam-previous-line-start)
   ;; ("C-M-k" sam-scroll-page-down)
   ;; ("C-M-k" holymotion-previous-visual-line)

   ("m" point-to-register)
   ("<C-m>" sam-match)

   ;;  "C-n" #'evil-paste-pop-next
   ("n"  consult-line-repeat)

   ;;  "A-o" #'sam-open-between
   ("C-o" gumshoe-buf-backtrack)
   ;; ("M-o" gumshoe-persp-backtrack)
   ("C-M-o" gumshoe-backtrack-back)
   ;;  "C-S-o" #'gumshoe-peruse-in-buffer
   ;;  "M-S-o" #'gumshoe-peruse-in-persp
   ;;  "C-M-S-o" #'gumshoe-peruse-globally

   ("C-p" ,perspective-map)

   ;;  "C-r" #'iedit-mode
   ("M-r" anzu-query-replace-regexp)

   ("s" sam-avy)
   ("C-s C-s" ctrlf-forward-symbol-at-point)
   ("C-s C-f" ctrlf-forward-fuzzy)
   ("C-s C-b" ctrlf-backward-fuzzy)
   ;;  "C-M-s" #'ctrlf-backward-fuzzy
   ;;  "M-s" nil
   ;;  "M-s M-s" #'ctrlf-occur
   ;;  "M-s M-f" #'ctrlf-forward-regexp
   ;;  "M-s M-b" #'ctrlf-backward-regexp

   ("t" ,+tab-keymap)
   ("C-t" xref-go-back)

   ("C-w" ,+window-map)
   ;;  "A-w" #'evilem-motion-forward-WORD-begin
   ;;  "s w" #'evilem-motion-forward-WORD-begin

   ("M-x" execute-extended-command)
   ("X"  delete-pair)
   ))

;; (general-define-key
;;  :states 'visual
;;  :keymaps root-modes
;;  "d" #'kill-region
;;  "y" #'copy-region-as-kill)

;; (general-define-key
;;  :states 'insert
;;  :keymaps '(shell-mode-map)
;;  "<tab>" #'completion-at-point)

;; (general-define-key
;;  :states 'normal
;;  :keymaps '(custom-mode-map Custom-mode-map)
;;  "<return>" #'sam-pushw-or-embark)

;; ;;; media keys
;; (general-define-key
;;  :states '(normal insert visual)
;;  :keymaps '(global)
;;  "<XF86Explorer>" #'sam-take-screenshot)

;; ;;; insert bindings
;; (general-define-key
;;  :states 'insert
;;  ;; Smarter newlines
;;  [remap newline] #'newline-and-indent  ; auto-indent on newline
;;  "C-v" 'yank
;;  "<mouse-2>" 'evil-paste-after)

;; (general-define-key
;;  :states 'insert
;;  :keymaps 'emacs-lisp-mode-map
;;  "<C-return>" #'eval-last-sexp)

;; (general-define-key
;;  :states 'visual
;;  :keymaps '(prog-mode-map conf-mode-map)
;;  ;; "d"   #'evil-delete
;;  "v"   #'er/expand-region
;;  ;; "C-j" #'evil-join
;;  "A-s" #'shell-command-on-region
;;  "<"   #'evil-shift-left
;;  ">"   #'evil-shift-right)

;; (general-define-key :states '(normal) "C-=" #'sam-indent-all)
;; (general-define-key :states '(visual) "=" #'indent-region)
;; (general-define-key
;;  :states 'normal
;;  :prefix "="
;;  ""  nil
;;  "=" #'evil-indent-line
;;  "s" #'indent-sexp
;;  "a" #'sam-indent-all
;;  "g" #'indent-rigidly)

;; ;; ported from vim-unimpaired
;; (general-define-key
;;  :states 'normal
;;  "[p"  #'evim-paste-above
;;  "]p"  #'evim-paste-below
;;  "]b"    #'next-buffer
;;  "[b"    #'previous-buffer)

;; ;; evil surround
;; (general-define-key
;;  :states 'visual
;;  :keymaps root-modes
;;  "o" #'evil-surround-region)

;; (general-define-key
;;  :states 'operator
;;  "s" #'evil-surround-edit
;;  "S" #'evil-Surround-edit)

;; ;;; evil override keybindings
;; ;; (evil-define-key 'normal magit-mode-map (kbd "C-d") 'dired-jump)

(setq-default ediff-mode-map (make-sparse-keymap))

;; (general-define-key
;;  :states '(normal insert visual)
;;  "<home>" help-map)

;; ;;; module keybinds
;; (general-define-key
;;  :keymaps 'embark-file-map
;;  "<return>" (my/embark-ace-action find-file)
;;  "C-h" (my/embark-split-action find-file windmove-display-left)
;;  "C-j" (my/embark-split-action find-file windmove-display-down)
;;  "C-k" (my/embark-split-action find-file windmove-display-up)
;;  "C-l" (my/embark-split-action find-file windmove-display-right))
;; (general-define-key
;;  :keymaps 'embark-symbol-map
;;  "RET" #'helpful-at-point
;;  "<C-return>" #'xref-find-definitions)

;; (with-eval-after-load 'debug
;;   (general-define-key
;;    :states 'normal
;;    :keymaps 'debugger-mode-map
;;    "SPC" +command-mode-map))
;; (general-define-key
;;  :states 'normal
;;  :keymaps 'markdown-mode-map
;;  "C-d" #'dired-jump)
;; (general-define-key
;;  :keymaps 'treemacs-mode-map
;;  :states 'normal
;;  "-" #'treemacs-quit)
;; (general-define-key
;;  :states 'normal
;;  :keymaps 'view-mode-map
;;  "e" #'view-mode)

;; (general-define-key
;;  :states 'normal
;;  :keymaps 'outline-mode-map
;;  "C-;" #'eval-expression)

;; (general-define-key
;;  :states 'normal
;;  :keymaps '(Info-mode-map)
;;  "C-n" #'Info-forward-node
;;  "C-p" #'Info-backward-node
;;  "C-h" #'Info-history-back
;;  "C-j" #'evil-scroll-line-down
;;  "C-k" #'evil-scroll-line-up
;;  "C-l" #'Info-history-forward
;;  "M-h" #'Info-prev
;;  "M-k" #'Info-up
;;  "M-l" #'Info-next
;;  "M-w" #'Info-next-reference
;;  "M-b" #'Info-prev-reference)

;; ;;; lang bindings
;; ;;(require 'evil-easymotion)
;; (evilem-make-motion
;; evilem-motion-next-sexp #'sp-next-sexp)
;; (evilem-make-motion
;; evilem-motion-backward-sexp #'sp-backward-sexp)
;; (general-define-key
;;  :states 'normal
;;  :keymaps '(lisp-mode-map lisp-interaction-mode-map emacs-lisp-mode-map)
;;  :prefix "C-e"
;;  ""   nil
;;  "C-e" #'sam-eval-this-sexp
;;  "C-b" #'eval-buffer
;;  "A-M-w" #'evilem-motion-next-sexp
;;  "s M-w" #'evilem-motion-next-sexp
;;  "A-M-b" #'evilem-motion-backward-sexp
;;  "s M-b" #'evilem-motion-backward-sexp
;;  )
;; (general-define-key
;;  :states 'visual
;;  :keymaps '(lisp-mode-map lisp-interaction-mode-map emacs-lisp-mode-map)
;;  "C-e" #'eval-region)

;; (general-define-key
;;  :states '(normal insert)
;;  :keymaps 'sh-mode-map
;;  "<C-return>" #'shelldon-send-line-at-point)
;; (general-define-key
;;  :states 'visual
;;  :keymaps 'sh-mode-map
;;  "C-e" #'shelldon-send-region)

(skey-define-keys
 '(ctl-x-map)
 `(
   ("C-b" persp-switch-to-buffer*)
   ("C-p" sam-project-persp-switch-project)
   ))

(skey-define-keys
 '(shelldon-mode-map)
 `(
   ("<return>" term-send-raw))
 )
;; (general-define-key
;;  :keymaps 'shelldon-minibuffer-local-command-map
;;  "<home>" help-map
;;  "M-SPC" +command-mode-map
;;  "C-l" #'completion-at-point)

;; (general-define-key
;;  :states 'normal
;;  :keymaps 'clojure-mode-map
;;  :prefix "C-e"
;;  ""   nil
;;  "C-e" #'cider-eval-sexp-at-point
;;  "C-f" #'cider-eval-file
;;  "C-b" #'cider-eval-buffer)

;;; completion
;; (general-define-key
;;  :keymaps 'prog-mode-map
;;  "<tab>" #'completion-at-point)
;; (general-define-key
;;  :keymaps root-modes
;;  :states 'insert
;;  "C-l" #'completion-at-point
;;  "M-l" #'tempel-expand)
;; (general-define-key
;;  :keymaps '(prog-mode-map org-mode-map markdown-mode-map)
;;  :states 'insert
;;  "`" (lambda () (interactive) (insert "`")))
(skey-define-keys
 '(global-gumshoe-backtracking-mode-map)
 `(
   ("C-o" gumshoe-backtrack-back)
   ("C-i" gumshoe-backtrack-forward)
   ))
(skey-define-keys
 '(corfu-map)
 '(("SPC" corfu-insert-separator)
   ("C-j" corfu-next)
   ("C-k" corfu-previous)
   ("C-l" corfu-complete)))
;; (general-define-key
;;  :keymaps 'tempel-map
;;  :states 'insert
;;  "<tab>" #'tempel-next
;;  "<backtab>" #'tempel-previous)

(skey-define-keys
 '(dired-mode-map)
 `(
   ;; ("<tab>" dired-subtree-toggle)
   (";"   execute-extended-command)
   ;; ("C-/" dired-narrow)
   ("C-b" ,+buffer-keymap)
   ("f" find-file)
   ("C-f" consult-line)
   ("C-l" dired-do-symlink)
   ("C-;" eval-expression)
   ("M-f" find-file)
   ("h"   dired-up-directory)
   ("j"   dired-next-line)
   ("k"   dired-previous-line)
   ("l"   dired-find-file)
   ("p"   sam-dired-yank-here)
   ("C-p" nil)
   ("C-p" ,perspective-map)
   ("y"   sam-dired-kill-path-at-point)
   ))

(require 'term)

(skey-define-keys
 '(term-raw-map)
 `(
   ("M-x" execute-extended-command)
   ("M-w" backward-kill-word)
   ;; ("C-x C-b" persp-switch-to-buffer*)
   ;; ("C-x C-p" persp-switch)
   ;; ("<tab>" evil-collection-term-send-tab)
   ;; ("C-l" evil-collection-term-send-tab)
   ("SPC" term-send-raw)
   ("<tab>" (lambda () (interactive) (term-send-raw-string "\t")))
   ("C-a" term-send-home)
   ("C-b" term-send-raw)
   ;; "C-e" #'term-send-end
   ;; "C-p" #'term-send-up
   ("C-v" term-primary-yank)
   ;; "C-n" #'term-send-down
   ;; "C-h" #'term-send-backspace
   )
 )

(skey-define-keys
 '(term-mode-map)
 `(
   ;; ("<tab>" evil-collection-term-send-tab)
   ;; ("C-l" evil-collection-term-send-tab)
   (";" execute-extended-command)
   ("C-v" consult-yank-from-kill-ring)
   ("C-b" ,+buffer-keymap)
   ("SPC" ,+command-mode-map)
   ("<tab>" (lambda () (interactive) (insert "\t")))
   ("C-w" ,+window-map)
   ;; "C-a" #'term-send-home
   ;; "C-e" #'term-send-end
   ;; "C-p" #'term-send-up
   ;; "C-n" #'term-send-down
   ;; "C-h" #'term-send-backspace
   )
 )

;; (general-define-key
;;  :keymaps '(smerge-mode-map)
;;  :states  '(normal)
;;  "<return>" #'smerge-keep-current)

;; ;;(evil-define-key 'normal
;; ;;  sclang-mode-map
;; ;;  "\C-e" #'+sclang-eval-this-expression)


;; ;;(defhydra evil-window-hydra (:color yellow :hint nil)
;; ;;  "
;; ;; _h_: window-left  _j_: window-down _k_: window-up _l_: window-right "
;; ;;  ("h"   evil-window-left)
;; ;;  ("j"   evil-window-down)
;; ;;  ("k"   evil-window-up)
;; ;;  ("l"   evil-window-right)
;; ;;  ("q"   nil "quit" :color blue))
;; ;;
;; ;;(general-define-key
;; ;; :keymaps 'evil-window-map
;; ;; "h" #'evil-window-hydra/evil-window-left
;; ;; "k" #'evil-window-hydra/evil-window-up
;; ;; "j" #'evil-window-hydra/evil-window-down
;; ;; "l" #'evil-window-hydra/evil-window-right)

;; ;;; tools
(skey-define-keys
 '(compilation-minor-mode-map)
 `(
   ("<M-return>" compilation-display-error)))
(skey-define-keys
 '(compilation-mode-map)
 `(
   ("<return>" compilation-display-error)))
;; (general-define-key
;;  :keymaps 'compilation-mode-map
;;  :states 'normal
;;  "<return>" #'compilation-display-error
;;  "M-j" #'compilation-next-error
;;  "M-k" #'compilation-previous-error)
;; (general-define-key
;;  :keymaps 'evil-magit
;;  ;; fix conflicts with private bindings
;;  :map '(gnus-summary-mode gnus-article-mode)
;;  "SPC" nil)

;; (general-define-key
;;  :keymaps '(gnus-mode-map gnus-topic-mode-map gnus-group-mode-map gnus-browse-mode-map gnus-summary-mode-map gnus-article-mode-map)
;;  "SPC" +command-mode-map)

(evim-define-default-derived-modes 'magit-blame)
(add-hook 'magit-blame-mode-hook #'evim-normal-magit-blame-mode)

(skey-define-keys
 '(evim-normal-magit-blame-mode-map magit-mode-map magit-diff-mode-map magit-status-mode-map magit-revision-mode-map)
 ;; "<return>" #'magit-visit-thing
 `(
   ("<C-return>" ,slink-keymap)
   ("<tab>" magit-section-toggle)
   ("SPC" ,+command-mode-map)
   ("C-b" ,+buffer-keymap)
   ("C-d" dired-jump)
   ("C-w" ,+window-map)
   ))

(skey-define-keys
 '(evim-normal-magit-blame-mode-map magit-blame-mode-map magit-blame-read-only-mode-map)
 `(("<return>" magit-show-commit)
   ("SPC" ,+command-mode-map)))
;; (skey-define-keys
;;  '(magit-diff-mode-map)
;;  `(("J" evil-scroll-page-down
;;     "K" evil-scroll-page-up)))
(skey-define-keys
 '(magit-status-mode-map)
 `(("M-;" shelldon)))

;; '(lisp-mode-map emacs-lisp-mode-map)
(skey-define-keys
 '(evim-normal-mode-map evim-visual-mode-map)
 `(
   ("("   backward-up-list)
   ("M-9" paredit-wrap-round)
   ;; ("M-9" sp-wrap)
   (")"   sp-up-sexp)
   ("C-h" sp-backward-slurp-sexp)
   ("C-l" sp-forward-slurp-sexp)
   ("M-d" kill-sexp)
   ("M-h" back-to-indentation)
   ("M-j" scroll-up-line)
   ("M-k" scroll-down-line)
   ("M-l" end-of-line)
   ("M-w" sp-next-sexp)
   ("M-e" sp-forward-sexp)
   ("M-b" sp-backward-sexp)
   ))

(skey-define-keys
 '(evim-normal-lisp-mode-map evim-insert-lisp-mode-map)
 `(
   ("M-I" sam-insert-at-beginning-of-form)
   ("M-A" sam-insert-at-end-of-form)))

(skey-define-keys
 '(evim-normal-term-mode-map)
 `(
   ("J" sam-scroll-up-command)
   ("K" sam-scroll-down-command)))

(skey-define-keys
 '(evim-insert-term-mode-map)
 `(
   ("C-v" (lambda ()
            (interactive)
            (term-send-raw-string (consult--read-from-kill-ring))))))

(skey-define-keys
 '(evim-insert-lisp-mode-map)
 `(
   ("C-v" consult-yank-from-kill-ring)
   ("M-j" sam-sexp-spawn-below)
   ("M-l" sam-sexp-eject-right)
   ("M-p" sam-sexp-reparent)))

;; (general-define-key
;;  :states 'normal
;;  :keymaps '(lisp-mode-map emacs-lisp-mode-map)
;;  ;; (kbd "M-<tab>") #'parinfer-toggle-mode
;;  "M-p" #'evil-cp-copy-paste-form
;;  "M-y" #'evil-cp-yank-sexp
;;  "M-d" #'evil-cp-delete-sexp
;;  ;; "M-h" #'evil-digit-argument-or-evil-beginning-of-line
;;  "H"   #'evil-window-top
;;  "L"   #'evil-window-bottom
;;  "w"   #'forward-to-word
;;  "e"   #'forward-word
;;  "b"   #'backward-word
;;  "M-9" #'paredit-wrap-sexp
;;  "M-0" #'sp-unwrap-sexp)

;; (general-define-key
;;  :states 'normal
;;  :keymaps 'python-mode-map
;;  "C-e C-b" #'python-shell-send-buffer
;;  "C-e C-e" #'python-shell-send-statement
;;  "C-e C-f" #'python-shell-send-defun
;;  "<return>" #'lsp-describe-thing-at-point)
;; (general-define-key
;;  :states 'visual
;;  :keymaps 'python-mode-map
;;  "C-e" #'python-shell-send-region)

;; (general-define-key
;;  :states 'normal
;;  :keymaps 'conf-mode-map
;;  "J" #'evil-scroll-page-down
;;  "K" #'evil-scroll-page-up
;;  )

;; (general-define-key
;;  :states 'normal
;;  :keymaps 'sexpy-mode-map
;;  "C-M-w"   #'sam-drag-sexp-forward
;;  "C-M-b"   #'sam-drag-sexp-backward)

;; (general-define-key
;;  :states 'normal
;;  :keymaps 'fundamental-mode-map
;;  "C-d" #'dired-jump)

;; (general-define-key
;;  :states 'insert
;;  :keymaps 'prog-mode-map
;;  "<backtab>" #'indent-rigidly-left
;;  "<tab>"     #'indent-relative
;;  "M-<tab>"   #'indent-relative-below)
;; (general-define-key
;;  :states '(insert)
;;  :keymaps 'prog-mode-map
;;  "<return>" #'newline-and-indent)

;; (general-define-key
;;  :states 'normal
;;  :keymaps 'org-mode-map
;;  "C-j"     #'evil-scroll-line-down
;;  "C-k"     #'evil-scroll-line-up
;;  "<tab>"   #'org-hide-entry
;;  "<backtab>" #'org-show-entry)
(require 'org)
(skey-define-keys
 '(org-mode-map)
 `(("<tab>"   org-do-demote)
   ("<backtab>" org-do-promote)))

;; (general-define-key
;;  :keymaps 'global
;;  :states 'insert
;;  "M-SPC" metallic-keymap)

;;; Language-specific keymaps

(require 'cc-mode)

(skey-define-keys
 '(c-mode-map)
 `(
   ;; ("<tab>"   (lambda () (interactive) (insert "\t")))
   ("<tab>"   nil)
   ))

(skey-define-keys
 +minibuffer-maps
 `(("C-g"    abort-recursive-edit)
   ("C-M-h"  ,help-map)
   ("C-v"    consult-yank-from-kill-ring)
   ("M-v"    sam-insert-path)
   ("C-a"    beginning-of-line)
   ("C-e"    end-of-line)
   ("C-f"    forward-char)
   ("C-b"    backward-char)
   ("C-g"    abort-recursive-edit)
   ("C-h"    backward-delete-char-untabify)
   ("C-M-h"  kill-whole-line)
   ("C-l"    completion-at-point)
   ("M-h"    sp-backward-delete-symbol)
   ("M-w"    forward-word)
   ("M-w" backward-kill-word)
   ("M-b"    backward-word)
   ("C-r" sam-minibuffer-history)
   ("C-p" previous-history-element)
   ("C-n" next-history-element)
   ("C-j" next-line-or-history-element)
   ("C-k" previous-line-or-history-element)
   ("M-n" next-line-or-history-element)
   ("M-p" previous-line-or-history-element)))

;; (defvar +minibuffer-completion-maps
;;   '(ivy-minibuffer-map vertico-map selectrum-minibuffer-map minibuffer-local-completion-map minibuffer-local-must-match-map)
;;   "A list of all the keymaps used for minibuffer completion.")
;; (general-define-key
;;  :keymaps +minibuffer-completion-maps
;;  "<tab>" #'minibuffer-complete
;;  "<home>" help-map
;;  "<C-space>" +command-mode-map
;;  "<C-return>" #'embark-act
;;  "C-v" #'yank
;;  "C-a" #'beginning-of-line
;;  "C-e" #'end-of-line
;;  "C-f" #'forward-char
;;  "C-b" #'backward-char
;;  "C-g" #'abort-recursive-edit
;;  "C-h" #'backward-delete-char-untabify
;;  "M-h" #'sp-backward-delete-symbol
;;  "C-M-h" #'kill-whole-line
;;  "M-w" #'forward-word
;;  "M-b" #'backward-word
;;  "C-j" #'next-line
;;  "C-k" #'previous-line)
;; (general-define-key
;;  :keymaps 'flyspell-mouse-map
;;  "<return>" #'flyspell-correct-at-point)
;; (general-define-key
;;  :keymaps 'popup-menu-keymap
;;  "<return>" #'popup-select)
;; (general-define-key
;;  :keymaps 'selectrum-minibuffer-map
;;  "<home>" help-map
;;  "<tab>" #'selectrum-insert-current-candidate
;;  "C-l"   #'selectrum-insert-current-candidate)
(skey-define-keys
 '(vertico-map)
 `(
   ("<home>" ,help-map)
   ("C-r" sam-minibuffer-history)
   ("C-p" previous-history-element)
   ("C-n" next-history-element)
   ("<tab>" vertico-insert)
   ("C-l"   vertico-insert)
   ))


;; (general-define-key
;;  :states 'normal
;;  :keymaps '(help-mode-map helpful-mode-map)
;;  "q" #'quit-window
;;  "M-b" #'backward-button
;;  "M-w" #'forward-button)

;; (general-define-key
;;  :states '(insert visual)
;;  :keymaps '(global prog-mode-map)
;;  "<C-[>" #'evil-force-normal-state)

;; (general-define-key
;;  :keymaps 'emacs-lisp-mode-map
;;  :states '(insert)
;;  "C-l" #'completion-at-point
;;  "M-l" #'tempel-complete)

(skey-define-keys
 '(embark-file-map)
 `(
   ("<return>" (lambda (_) (interactive) (call-interactively #'find-file)))
   ("f" find-file)))

(skey-define-keys
 +all-maps
 `(
   ("M-;" shelldon)
   ("C-;" eval-expression)
   ("C-w" nil)
   ("C-w" ,+window-map)))

(skey-define-keys
 (cl-set-difference +all-maps (append +repl-maps +minibuffer-maps '(evim-insert-mode-map)))
 `(
   ("?" which-key-show-top-level)
   (";" execute-extended-command)
   ("C-SPC" ,+controller-keymap)
   ("M-SPC" ,+metallic-keymap)
   ("S-SPC" ,+shifty-keymap)
   ("A-SPC" ,+alter-keymap)
   ("SPC" ,+command-mode-map)
   ))

(provide '+bindings)
;;; +bindings.el ends here
