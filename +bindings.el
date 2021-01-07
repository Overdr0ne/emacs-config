;;; config/default/+bindings.el -*- lexical-binding: t; -*-

;; This file defines a Spacemacs-esque keybinding scheme

;; Don't let evil-collection interfere with certain keys
(setq evil-collection-key-blacklist
      (list "<escape>"))

(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "M-p") 'term-primary-yank)))

(general-define-key
 :states 'normal
 :prefix "C-e"
 ""	 nil
 "C-e" #'+eval-this-sexp
 "C-b" #'eval-buffer)

;; (general-define-key
;;  :states 'normal
;;  "1" #'evil-shell-command
;;  "2" #'evil-execute-macro
;;  "3" #'evil-ex-search-word-backward
;;  "4" #'evil-end-of-line
;;  "5" #'evil-jump-item
;;  "6" #'evil-first-non-blank
;;  "7" #'evil-ex-repeat-substitute
;;  "8" #'swiper-isearch-thing-at-point
;;  "9" #'evil-backward-sentence-begin
;;  "0" #'evil-beginning-of-line)

;; (general-define-key
;;  :states 'normal
;;  :keymaps 'evil-cleverparens-mode-map
;;  "9" #'evil-cp-backward-up-sexp
;;  "0" #'evil-cp-up-sexp)

;;; Global keybindings
(general-define-key
 ;; [remap quit-window] #'kill-current-buffer
 ;; "A-b"  #'persp-ibuffer
 ;; "A-p"  #'sam-projectile-ibuffer
 "M-;" #'eval-expression
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
 :keymaps '(global evil-cleverparens-mode-map)
 ;; "TAB" #'evil-toggle-fold
 "M-w" #'forward-word

 "M-j" #'(lambda () (interactive)
           (progn (evil-next-line)
                  (back-to-indentation)))
 "M-k" #'(lambda () (interactive)
           (progn (evil-previous-line)
                  (back-to-indentation)))

 "j"   #'evil-next-visual-line
 "k"   #'evil-previous-visual-line

 "C-j"  #'evil-scroll-line-down
 "C-k"  #'evil-scroll-line-up

 "C-h"  help-map
 "n"  #'isearch-repeat-forward
 "N"  #'isearch-repeat-backward
 "K"  #'delete-indentation
 "J"  #'evil-join
 "t"   #'(lambda () (interactive) (avy-goto-word-or-subword-1))
 "C"   #'+multiple-cursors/evil-mc-toggle-cursors
 "<C-return>" #'sam-hs-toggle-all
 "C-d" #'dired-jump
 "C-u" #'universal-argument
 "C-q" #'evil-quit
 "C-w" #'evil-window-map
 "C-/" #'counsel-grep-or-swiper
 "C-]" #'xref-find-definitions
 "M-]" #'xref-find-references
 ","   #'macrostep-expand
 "-"   #'treemacs-add-and-display-current-project
 "/"   #'swiper-isearch)

(general-define-key
 :states 'insert
 "C-a"           #'beginning-of-line
 "C-e"           #'end-of-line
 "C-f"           #'forward-char
 "C-b"           #'backward-char
 "C-n"           #'next-line
 "C-p"           #'previous-line
 "C-h"           #'backward-delete-char-untabify
 "M-w"           #'forward-word
 "M-b"           #'backward-word
 ;; Smarter newlines
 [remap newline] #'newline-and-indent  ; auto-indent on newline
 "C-j"           #'+default/newline    ; default behavior
 "C-v" 'yank
 "<mouse-2>" 'evil-paste-after)

(general-define-key
 :states '(normal visual)
 :keymaps '(global evil-cleverparens-mode-map)
 "w"  #'forward-to-word
 "e"  #'forward-word
 "b"  #'backward-word
 "z=" #'flyspell-correct-word-generic
 ";"   #'(lambda () (interactive) (counsel-M-x ""))
 )

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
(general-define-key
 :states 'normal
 :keymaps '(global evil-cleverparens-mode-map)
 (kbd "u") 'undo
 ;; (kbd "U") 'hydra-undo-tree/undo-tree-redo
 ;; (kbd "u") 'hydra-undo-tree/undo-tree-undo
 )

(general-define-key
 :states 'visual
 "v"   #'er/expand-region
 "C-j" #'evil-join
 "A-s" #'shell-command-on-region
 "x"   #'+multiple-cursors/evil-mc-make-cursor-here
 "@"   #'+evil:apply-macro
 "<"     #'+evil/visual-dedent
 ">"     #'+evil/visual-indent)

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
 "] SPC" #'+evil/insert-newline-below
 "[ SPC" #'+evil/insert-newline-above
 "]b"    #'next-buffer
 "[b"    #'previous-buffer
 "]f"    #'+evil/next-file
 "[f"    #'+evil/previous-file)

(general-define-key
 :states 'motion
 "]u"    #'+evil:url-encode
 "[u"    #'+evil:url-decode
 "]y"    #'+evil:c-string-encode
 "[y"    #'+evil:c-string-decode)

;; evil surround
(general-define-key
 :states 'visual
 "S" #'evil-surround-region)

(general-define-key
 :states 'operator
 "s" #'evil-surround-edit
 "S" #'evil-Surround-edit)

;; easymotion
(evilem-define (kbd "A-w") 'forward-to-word :scope 'line)
(evilem-define (kbd "A-b") 'backward-word :scope 'line)
(evilem-define (kbd "A-e") 'forward-word :scope 'line)
(evilem-define (kbd "A-j") 'evil-next-visual-line)
(evilem-define (kbd "A-k") 'evil-previous-visual-line)
(evilem-define (kbd "A-l") 'evil-forward-char :scope 'word)
(evilem-define (kbd "A-h") 'evil-backward-char :scope 'word)
;; (evilem-define (kbd "f") 'evil-snipe-f)

;;; Evil override keybindings
(evil-define-key 'normal magit-mode-map (kbd "C-d") 'dired-jump)

;;
;;; Module keybinds

(general-define-key
 :keymaps 'treemacs-mode-map
 :states 'normal
 "-" #'treemacs-quit)

;; ivy
(general-define-key
 :map ivy-minibuffer-map
 "M-h"   #'ivy-backward-kill-word
 "C-h"   #'ivy-backward-delete-char
 "C-l"   #'ivy-alt-done
 "C-v"   #'yank
 "C-n" #'next-line
 "C-p" #'previous-line)

;;; :completion
(general-define-key
 :states 'insert
 :prefix "C-SPC"
 ;; "C-e"    #'+eval-this-sexp
 "C-SPC"  #'company-complete
 ;; "C-l"    #'+company/whole-lines
 ;; "C-k"    #'+company/dict-or-keywords
 "C-f"    #'company-files
 "C-]"    #'company-etags
 "s"      #'company-ispell
 "C-s"    #'company-yasnippet
 "C-o"    #'company-capf)
;; "C-n"    #'+company/dabbrev
;; "C-p"    #'+company/dabbrev-code-previous

(general-define-key
 :keymaps 'company-active-map
 (kbd "C-n") #'company-select-next
 (kbd "C-p") #'company-select-previous)

;;(unbind-key "y" dired-mode-map)
(general-define-key
 :keymaps 'dired-mode-map
 :state 'normal
 :prefix "y"
 ""    nil
 "y"   #'dired-ranger-copy
 "m"   #'dired-ranger-move
 "p"   #'dired-ranger-paste)
(general-define-key
 :keymaps 'dired-mode-map
 :states 'normal
 ""      nil
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

(evil-define-key 'normal
  sclang-mode-map
  "\C-e" #'+sclang-eval-this-expression)

;;; :ui
(general-define-key
 :keymaps '(evil-window-map)
 ;; Navigation
 "C-h"     #'evil-window-left
 "C-j"     #'evil-window-down
 "C-k"     #'evil-window-up
 "C-l"     #'evil-window-right
 "C-q"     #'delete-window
 "C-t"     #'window-toggle-side-windows
 "C-w"     #'other-window
 "C-n"     #'evil-window-vnew
 ;; Swapping windows
 "H"       #'+evil/window-move-left
 "J"       #'+evil/window-move-down
 "K"       #'+evil/window-move-up
 "L"       #'+evil/window-move-right
 "C-a"   #'ace-swap-window
 "u"       #'winner-undo
 "C-u"     #'winner-undo
 "C-r"     #'winner-redo
 "o"       #'delete-other-windows
 ;; Delete window
 "C-C"     #'ace-delete-window)

;;; :editor
;;(map!
;; ;; (:when (featurep! :editor fold)
;; ;;   :nv "A-SPC" #'(lambda () (interactive) (toggle-fold-lines))
;; ;;   :nv "C-SPC" #'+fold/toggle)
;;
;; (:when (featurep! :editor format)
;;   :n "gQ"    #'+format:region)
;;
;; (:when (featurep! :editor multiple-cursors)
;;   ;; evil-mc
;;   (:prefix "gz"
;;            :nv "d" #'evil-mc-make-and-goto-next-match
;;            :nv "D" #'evil-mc-make-and-goto-prev-match
;;            :nv "j" #'evil-mc-make-cursor-move-next-line
;;            :nv "k" #'evil-mc-make-cursor-move-prev-line
;;            :nv "m" #'evil-mc-make-all-cursors
;;            :nv "n" #'evil-mc-make-and-goto-next-cursor
;;            :nv "N" #'evil-mc-make-and-goto-last-cursor
;;            :nv "p" #'evil-mc-make-and-goto-prev-cursor
;;            :nv "P" #'evil-mc-make-and-goto-first-cursor
;;            :nv "q" #'evil-mc-undo-all-cursors
;;            :nv "t" #'+multiple-cursors/evil-mc-toggle-cursors
;;            :nv "u" #'evil-mc-undo-last-added-cursor
;;            :nv "z" #'+multiple-cursors/evil-mc-make-cursor-here)
;;   (:after evil-mc
;;           :map evil-mc-key-map
;;           :nv "C-n" #'evil-mc-make-and-goto-next-cursor
;;           :nv "C-N" #'evil-mc-make-and-goto-last-cursor
;;           :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
;;           :nv "C-P" #'evil-mc-make-and-goto-first-cursor)
;;   ;; evil-multiedit
;;   :v  "R"     #'evil-multiedit-match-all
;;   :n  "M-d"   #'evil-multiedit-match-symbol-and-next
;;   :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
;;   :v  "M-d"   #'evil-multiedit-match-and-next
;;   :v  "M-D"   #'evil-multiedit-match-and-prev
;;   :nv "C-M-d" #'evil-multiedit-restore
;;   (:after evil-multiedit
;;           (:map evil-multiedit-state-map
;;                 "M-d"    #'evil-multiedit-match-and-next
;;                 "M-D"    #'evil-multiedit-match-and-prev
;;                 "RET"    #'evil-multiedit-toggle-or-restrict-region
;;                 [return] #'evil-multiedit-toggle-or-restrict-region)
;;           (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
;;                 "C-n" #'evil-multiedit-next
;;                 "C-p" #'evil-multiedit-prev)))
;;
;; (:when (featurep! :editor rotate-text)
;;   :n "!" #'rotate-text)
;;
;; (:when (featurep! :editor snippets)
;;   ;; auto-yasnippet
;;   :i  [C-tab] #'aya-expand
;;   :nv [C-tab] #'aya-create
;;   ;; yasnippet
;;   (:after yasnippet
;;           (:map yas-keymap
;;                 "C-e"         #'+snippets/goto-end-of-field
;;                 "C-a"         #'+snippets/goto-start-of-field
;;                 [M-right]     #'+snippets/goto-end-of-field
;;                 [M-left]      #'+snippets/goto-start-of-field
;;                 [M-backspace] #'+snippets/delete-to-start-of-field
;;                 [backspace]   #'+snippets/delete-backward-char
;;                 [delete]      #'+snippets/delete-forward-char-or-field))))

;; (
;; Add vimish-fold, outline-mode & hideshow support to folding commands
;; (define-key! 'global
;;   [remap evil-toggle-fold]   #'+fold/toggle
;;   [remap evil-close-fold]    #'+fold/toggle
;;   [remap evil-open-fold]     #'+fold/open
;;   [remap evil-open-fold-rec] #'+fold/open
;;   [remap evil-close-folds]   #'+fold/close-all
;;   [remap evil-open-folds]    #'+fold/open-all)
;; (evil-define-key* 'motion 'global
;;   "zm" #'toggle-fold-lines)
;; )

;;; :tools
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
 "M-p"     #'evil-cp-copy-paste-form
 "M-y"     #'evil-cp-yank-sexp
 "M-d"     #'evil-cp-delete-sexp
 "H"       #'evil-window-top
 "L"       #'evil-window-bottom
 "M-9"     #'paredit-wrap-sexp
 "M-0"     #'sp-unwrap-sexp
 "S"       #'evil-snipe-S
 "s"       #'evil-snipe-s)

(general-define-key
 :states 'insert
 :keymaps 'evil-cleverparens-mode-map
 "<tab>"     #'indent-relative
 "<backtab>" #'indent-relative-below)

(general-define-key
 :states '(normal visual)
 :keymaps 'prog-mode-map
 "RET" #'evil-commentary-line)

(general-define-key
 :states 'normal
 :keymaps 'evil-org-mode-map
 "<return>" #'org-toggle-narrow-to-subtree
 "t" 	    #'org-set-tags-command
 "pp" 	    #'evil-paste-after
 "pl" 	    #'org-insert-link)
(general-define-key
 :states 'insert
 :keymaps 'evil-org-mode-map
 "<tab>"     #'org-do-demote
 "<backtab>" #'org-do-promote)

(setq light-theme t)

(general-define-key
 :keymaps '(global magit-mode-map dired-mode-map helpful-mode-map help-mode-map)
 :states '(normal visual insert)
 :non-normal-prefix "M-SPC"
 :prefix "SPC"
 ""     nil
 "-"    #'ranger
 ";"    #'amx
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
 "C-SPC" #'tmm-menubar
 "TAB" #'mode-line-other-buffer

 "a" '(:ignore t :which-key "admin")
 "aa" #'helm-system-packages
 "ad" #'daemons
 "ae" #'counsel-package
 "al" #'list-packages
 "ap" #'proced
 "as" #'helm-systemd

 "b"   #'persp-counsel-switch-buffer
 "B"   #'counsel-switch-buffer
 "M-b" #'persp-ibuffer
 "A-b" #'ibuffer
 "["   #'previous-buffer
 "]"   #'next-buffer

 "C-c" #'sfs-recollect
 "c" '(:ignore t :which-key "code")
 "cc"   #'sam-compile
 "ce"   #'+eval/buffer-or-region
 "cE"   #'+eval:replace-region
 "cf"   #'+format/region-or-buffer
 "cr"   #'+eval/open-repl-other-window
 "cw"   #'delete-trailing-whitespace
 "cx"   #'flycheck-list-errors

 "d" '(:ignore t :which-key "debug")
 "dd"   #'gdb
 "dm"   #'gdb-many-windows
 "ds"   #'serial-term

 "e" '(:ignore t :which-key "edit")
 "eb"   #'(lambda () (interactive) (find-file "~/.emacs.d/overdr0ne/+bindings.el"))
 "ez"   #'(lambda () (interactive) (find-file "~/.zshrc"))
 "eh"   #'(lambda () (interactive) (find-file "/etc/httpd/conf/httpd.conf"))
 "ei"   #'(lambda () (interactive) (find-file "~/.config/i3/config"))

 "C-f"  #'sfs-research
 "f"    #'counsel-find-file

 "g" '(:ignore t :which-key "go")
 "g."   #'(lambda () (interactive) (find-file "."))
 "gh"   #'(lambda () (interactive) (find-file "~"))
 "gl"   #'(lambda () (interactive) (find-file "~/src"))
 "gn"   #'(lambda () (interactive) (find-file "~/notes"))
 "go"   #'(lambda () (interactive) (find-file "~/.emacs.d/overdr0ne"))
 "gs"   #'(lambda () (interactive) (find-file "~/sites"))
 "gt"   #'(lambda () (interactive) (find-file "~/test"))

 "h" help-map

;;; <leader> i --- imenu
 "i" '(:ignore t :which-key "imenu")
 "if"   #'((lambda () (interactive) (sam-counsel-imenu "functions: ")) :which-key "functions")
 "ii"   #'counsel-imenu
 "im"   #'(lambda () (interactive) (sam-counsel-imenu "macros: "))
 "ip"   #'(lambda () (interactive) (sam-counsel-imenu "package "))
 "is"   #'(lambda () (interactive) (sam-counsel-imenu "section: "))
 "iv"   #'(lambda () (interactive) (sam-counsel-imenu "variables: "))
 "it"   #'(lambda () (interactive) (find-file "~/test"))

 "j"    #'(lambda () (interactive) (avy-goto-word-or-subword-1))

 ;; ;;; <leader> k --- kill
 "k" #'kill-this-buffer
 "C-k" #'kill-buffer-and-window
 ;; "k" '(:ignore t :which-key "kill")
 ;; "kb" #'kill-buffer
 ;; "kc" #'kill-buffer-and-window
 ;; "kk" #'kill-this-buffer
 ;; "kw" #'delete-window

 "l" '(:ignore t :which-key "lookup")
 "la" #'ace-link
 "lb" #'swiper
 "ld" #'counsel-rg
 "lf" #'counsel-file-jump
 "li" #'imenu
 "ll" #'swiper
 "lp" #'+default/search-project-for-symbol-at-point
 "lP" #'+default/search-other-project
 "ls" #'swiper
 "lw" #'wordnut-lookup-current-word
 "lx" #'sx-search

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
 "pp" #'persp-prev
 "pr" #'(lambda () (interactive) (find-file (projectile-project-root)))
 "pR" #'projectile-run-project
 "ps" #'persp-switch
 "pt" #'+default/project-tasks
 "pT" #'projectile-test-project

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

 "C-t" #'(lambda () (interactive)
           (setq light-theme (not light-theme))
           (if light-theme
               (load-theme 'spacemacs-light t)
             (load-theme 'dracula t)))

 "t" '(:ignore t :which-key "terminal")
 "tj" #'(lambda () (interactive) (multi-term-next))
 "tk" #'(lambda () (interactive) (multi-term-prev))
 "tt" #'(lambda () (interactive) (multi-term))
 "tr" #'+eval/open-repl-other-window
 "tR" #'+eval/open-repl-same-window
 "t;" #'(lambda () (interactive) (multi-term-dedicated-toggle))

 "u" '(:ignore t :which-key "utilities")
 "uc" #'sfs-recollect
 "ud" #'wordnut-search
 "uk" #'browse-kill-ring
 "ur" #'replace-string
 "uR" #'replace-query
 "us" #'sfs-research

 "v" '(:ignore t :which-key "version control")
 "v]"   #'git-gutter:next-hunk
 "v["   #'git-gutter:previous-hunk
 "v/"   #'magit-dispatch
 "v'"   #'forge-dispatch
 "va" '(:ignore t :which-key "create")
 "vai"   #'forge-create-issue
 "vap"   #'forge-create-pullreq
 "vb" '(:ignore t :which-key "branch")
 "vB"   #'magit-blame-addition
 "vbb"   #'magit-branch
 "vbc"   #'magit-branch-and-checkout
 "vc" '(:ignore t :which-key "commit")
 "vcc"   #'magit-commit-create
 "vcf"   #'magit-commit-fixup
 "vf" '(:ignore t :which-key "file")
 "vff"   #'magit-find-file
 "vfg"   #'magit-find-git-config-file
 "vfc"   #'magit-show-commit
 "vfi"   #'forge-visit-issue
 "vfp"   #'forge-visit-pullreq
 "vF"   #'magit-fetch
 "vh" '(:ignore t :which-key "checkout")
 "vhb"   #'magit-branch-checkout
 "vhh"   #'magit-checkout
 "vi"   #'magit-init
 "vl" '(:ignore t :which-key "list")
 "vlg"   #'+gist:list
 "vlr"   #'magit-list-repositories
 "vls"   #'magit-list-submodules
 "vli"   #'forge-list-issues
 "vlp"   #'forge-list-pullreqs
 "vln"   #'forge-list-notifications
 "vn"   #'magit-clone
 "vo" '(:ignore t :which-key "browse")
 "vo."   #'+vc/git-browse-region-or-line
 "vor"   #'forge-browse-remote
 "voc"   #'forge-browse-commit
 "voi"   #'forge-browse-issue
 "vop"   #'forge-browse-pullreq
 "voI"   #'forge-browse-issues
 "voP"   #'forge-browse-pullreqs
 "vr"   #'git-gutter:revert-hunk
 "vs"   #'git-gutter:stage-hunk
 "vt"   #'git-timemachine-toggle
 "vU"   #'magit-unstage-file
 "vv"   #'magit-status
 "vx"   #'magit-file-delete
 "vL"   #'magit-log
 "vR"   #'vc-revert
 "vS"   #'magit-stage-file

;;; <leader> w --- windows
 "w"    evil-window-map

;;; <leader> x --- org capture
 "x"    #'org-capture

;;; <leader> y --- hyperbole
 "y"    #'hyperbole

 "z" '(:ignore t :which-key "scratch")
 "zc" #'((lambda () (interactive) (find-file "~/scratch/c/test.c"))         :which-key "C")
 "zl" #'((lambda () (interactive) (find-file "~/scratch/elisp/test.el"))    :which-key "Elisp")
 "zp" #'((lambda () (interactive) (find-file "~/scratch/py/test.py"))       :which-key "Python")
 "zt" #'((lambda () (interactive) (find-file "~/scratch/text/generic.txt")) :which-key "text"))

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
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map
    read-expression-map
    ivy-minibuffer-map
    ivy-switch-buffer-map)
  "A list of all the keymaps used for the minibuffer.")
(general-define-key
 :keymaps +default-minibuffer-maps
 [escape] #'abort-recursive-edit
 "C-v"    #'yank
 ;; "C-a"    #'move-beginning-of-line
 ;; "C-b"    #'backward-word
 "C-a"           #'beginning-of-line
 "C-e"           #'end-of-line
 "C-f"           #'forward-char
 "C-b"           #'backward-char
 "M-w"           #'forward-word
 "M-b"           #'backward-word
 "C-r"    #'evil-paste-from-register
 ;; "C-n"    #'next-history-element
 ;; "C-p"    #'previous-history-element
 ;; Scrolling lines
 "C-j"    #'next-line
 "C-k"    #'previous-line
 "C-S-j"  #'scroll-up-command
 "C-S-k"  #'scroll-down-command)

(general-define-key
 :keymaps 'read-expression-map
 "C-j" #'next-line-or-history-element
 "C-k" #'previous-line-or-history-element)

(general-define-key
 :keymaps 'help-map
 ;; new keybinds
 "'"    #'describe-char
 ;; Unbind `help-for-help'. Conflicts with which-key's help command for the
 ;; <leader> h prefix. It's already on ? and F1 anyway.
 "C-h"  nil
 "a"    #'counsel-apropos
 "b"    #'helm-descbinds
 "c" 	#'helpful-command
 "C-c"  #'describe-coding-system
 "f"    #'helpful-callable
 "F"    #'counsel-describe-face
 "h"    #'helpful-at-point
 "k"    #'helpful-key
 "C-k"  #'describe-key-briefly
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

(defhydra evil-window-hydra (:color yellow
                                    :hint nil)
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
