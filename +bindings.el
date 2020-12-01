;;; config/default/+bindings.el -*- lexical-binding: t; -*-

;; This file defines a Spacemacs-esque keybinding scheme

;; Don't let evil-collection interfere with certain keys
(setq evil-collection-key-blacklist
      (list "<escape>"))

;; (defadvice! +default-evil-collection-disable-blacklist-a (orig-fn)
;;   :around #'evil-collection-vterm-toggle-send-escape  ; allow binding to ESC
;;   (let (evil-collection-key-blacklist)
;;     (apply orig-fn)))

(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "M-p") 'term-primary-yank)))

;; (evil-define-key '(normal insert) lispyville-mode-map
;;   (kbd "M-(") 'lispy-wrap-round
;;   (kbd "M-[") 'lispy-wrap-brackets
;;   (kbd "M-{") 'lispy-wrap-braces
;;   (kbd "C-)") 'lispy-forward-slurp-sexp
;;   (kbd "A-)") 'lispy-forward-barf-sexp
;;   (kbd "C-(") 'lispy-backward-slurp-sexp
;;   (kbd "A-(") 'lispy-backward-barf-sexp
;;   (kbd "A-j") 'evil-scroll-line-down
;;   (kbd "A-k") 'evil-scroll-line-up)

;; (general-define-key
;;  :states 'normal
;;  :keymaps 'Info-mode-map
;;  )

(general-define-key
 :states 'normal
 :prefix "C-e"
 ""        nil
 "C-e" #'+eval-this-sexp
 "C-b" #'eval-buffer)

(general-define-key
 :states 'normal
 "1" #'rotate-text
 "2" #'evil-execute-macro
 "3" #'evil-ex-search-word-backward
 "4" #'evil-end-of-line
 "5" #'evil-jump-item
 "6" #'evil-first-non-blank
 "7" #'evil-ex-repeat-substitute
 "8" #'evil-ex-search-word-forward
 "9" #'evil-backward-sentence-begin
 "0" #'evil-beginning-of-line)

(general-define-key
 :states 'normal
 :keymaps 'evil-cleverparens-mode-map
 "9" #'evil-cp-backward-up-sexp
 "0" #'evil-cp-up-sexp)

;;
;;; Global keybindings
(general-define-key
 [remap quit-window] #'kill-current-buffer
 ;; "A-b"  #'persp-ibuffer
 ;; "A-p"  #'sam-projectile-ibuffer
 "M-;" #'eval-expression
 "<C-m>" #'evilmi-jump-items)

(general-define-key
 :states 'insert
 ;; [tab]            #'indent-relative
 ;; [backtab]        #'indent-relative-below
 "C-a"           #'beginning-of-line
 "C-e"           #'end-of-line
 "C-f"           #'forward-char
 "C-b"           #'backward-char
 "C-n"           #'next-line
 "C-p"           #'previous-line
 "C-h"           #'evil-delete-backward-char
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
 "K" #'delete-indentation
 "J" #'evil-join
 "j"   #'evil-next-visual-line
 "k"   #'evil-previous-visual-line
 "C-j"  #'evil-scroll-line-down
 "C-k"  #'evil-scroll-line-up
 "t"   #'(lambda () (interactive) (avy-goto-word-or-subword-1))
 "C"   #'+multiple-cursors/evil-mc-toggle-cursors
 ;; "`"   #'sr-speedbar-toggle
 "C-d" #'dired-jump
 ;; "C-j" #'insert-line-below
 ;; "C-k" #'insert-line-above
 "C-u" #'universal-argument
 "C-q" #'kill-current-buffer
 "C-w" #'evil-quit
 "C-/" #'counsel-grep-or-swiper
 "C-]" #'xref-find-definitions
 "M-]" #'xref-find-references
 ","   #'macrostep-expand
 "-"   #'treemacs
 "/"   #'swiper)

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
;; NOTE hl-todo-{next,previous} have ]t/[t, use ]F/[F instead
;; NOTE {next,previous}-error have ]e/[e, use ddp/ddP or gx instead
;; (:when (featurep! :lang web)
;;  "]x" #'+web:encode-html-entities
;;  "[x" #'+web:decode-html-entities)


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
 ""      nil
 "y"     #'dired-ranger-copy
 "m"     #'dired-ranger-move
 "p"     #'dired-ranger-paste)
(general-define-key
 :keymaps 'dired-mode-map
 :states 'normal
 ""      nil
 "\C-l" #'dired-do-symlink
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
;; (map! (:when (featurep! :ui hl-todo)
;;         :m "]t" #'hl-todo-next
;;         :m "[t" #'hl-todo-previous)

;;       (:when (featurep! :ui popup)
;;         :n "C-`"   #'+popup/toggle
;;         :n "C-~"   #'+popup/raise
;;         :g "C-x p" #'+popup/other)

;;       (:when (featurep! :ui vc-gutter)
;;         :m "]d"    #'git-gutter:next-hunk
;;         :m "[d"    #'git-gutter:previous-hunk)

;;     ;;; <leader> w --- windowing
;;       (:map evil-window-map
;;             ;; Navigation
;;             "C-h"     #'evil-window-left
;;             "C-j"     #'evil-window-down
;;             "C-k"     #'evil-window-up
;;             "C-l"     #'evil-window-right
;;             "C-w"     #'other-window
;;             "C-n"     #'evil-window-vnew
;;             "C-q"     #'evil-quit-all
;;             ;; Swapping windows
;;             "H"       #'+evil/window-move-left
;;             "J"       #'+evil/window-move-down
;;             "K"       #'+evil/window-move-up
;;             "L"       #'+evil/window-move-right
;;             "C-S-w"   #'ace-swap-window
;;             ;; Window undo/redo
;;             (:prefix "m"
;;                      "m"       #'doom/window-maximize-buffer
;;                      "v"       #'doom/window-maximize-vertically
;;                      "s"       #'doom/window-maximize-horizontally)
;;             (:prefix "C-m"
;;                      "m"       #'doom/window-maximize-buffer
;;                      "v"       #'doom/window-maximize-vertically
;;                      "s"       #'doom/window-maximize-horizontally)
;;             "u"       #'winner-undo
;;             "C-u"     #'winner-undo
;;             "C-r"     #'winner-redo
;;             ;; "o"       #'doom/window-enlargen
;;             "o"       #'delete-other-windows
;;             ;; Delete window
;;             "C-C"     #'ace-delete-window))

(general-define-key
 :keymaps '(evil-window-map)
 ;; Navigation
 "C-h"     #'evil-window-left
 "C-j"     #'evil-window-down
 "C-k"     #'evil-window-up
 "C-l"     #'evil-window-right
 "C-w"     #'other-window
 "C-n"     #'evil-window-vnew
 "C-q"     #'evil-quit-all
 ;; Swapping windows
 "H"       #'+evil/window-move-left
 "J"       #'+evil/window-move-down
 "K"       #'+evil/window-move-up
 "L"       #'+evil/window-move-right
 "C-S-w"   #'ace-swap-window
 ;; Window undo/redo
 ;; "m m"       #'doom/window-maximize-buffer
 ;; "m v"       #'doom/window-maximize-vertically
 ;; "m s"       #'doom/window-maximize-horizontally
 ;; "C-m m"       #'doom/window-maximize-buffer
 ;; "C-m v"       #'doom/window-maximize-vertically
 ;; "C-m s"       #'doom/window-maximize-horizontally
 "u"       #'winner-undo
 "C-u"     #'winner-undo
 "C-r"     #'winner-redo
 ;; "o"       #'doom/window-enlargen
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
 :states '(normal)
 :keymaps 'evil-org-mode-map
 "t" #'org-set-tags-command
 "pp" #'evil-paste-after
 "pl" #'org-insert-link)

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
 "TAB" #'mode-line-other-buffer

 "a" '(:ignore t :which-key "admin")
 "aa" #'helm-system-packages
 "ad" #'daemons
 "ae" #'counsel-package
 "al" #'list-packages
 "ap" #'proced
 "as" #'helm-systemd
 ;; :desc "arch packages"  "a"   #'helm-system-packages
 ;; :desc "daemons"        "d"   #'daemons
 ;; :desc "emacs packages" "e"   #'counsel-package
 ;; :desc "list-packages"  "l"   #'list-packages
 ;; :desc "proced"         "p"   #'proced
 ;; :desc "systemd"        "s"   #'helm-systemd

 "b"   #'persp-counsel-switch-buffer
 "B"   #'counsel-switch-buffer
 "M-b" #'persp-ibuffer
 "A-b" #'ibuffer
 "["   #'previous-buffer
 "]"   #'next-buffer

 "c" '(:ignore t :which-key "code")
 "cc"   #'sam-compile
 "ce"   #'+eval/buffer-or-region
 "cE"   #'+eval:replace-region
 "cf"   #'+format/region-or-buffer
 "cr"   #'+eval/open-repl-other-window
 "cw"   #'delete-trailing-whitespace
 "cW"   #'doom/delete-trailing-newlines
 "cx"   #'flycheck-list-errors

 "d" '(:ignore t :which-key "debug")
 "dd"   #'gdb
 "dm"   #'gdb-many-windows
 "ds"   #'serial-term

 "e" '(:ignore t :which-key "edit")
 "eb"   #'(lambda () (interactive) (find-file "~/.doom.d/+bindings.el"))
 "ez"   #'(lambda () (interactive) (find-file "~/.zshrc"))
 "eh"   #'(lambda () (interactive) (find-file "/etc/httpd/conf/httpd.conf"))
 "ei"   #'(lambda () (interactive) (find-file "~/.config/i3/config"))

 "f"    #'counsel-find-file

 "g" '(:ignore t :which-key "go")
 "g."   #'(lambda () (interactive) (find-file "."))
 "gd"   #'(lambda () (interactive) (find-file "~/.doom.d"))
 "gh"   #'(lambda () (interactive) (find-file "~"))
 "gl"   #'(lambda () (interactive) (find-file "~/src"))
 "gn"   #'(lambda () (interactive) (find-file "~/notes"))
 "gs"   #'(lambda () (interactive) (find-file "~/sites"))
 "gt"   #'(lambda () (interactive) (find-file "~/test"))

 "h" help-map

 "i" '(:ignore t :which-key "imenu")
 "if"   #'((lambda () (interactive) (sam-counsel-imenu "functions: ")) :which-key "functions")
 "ii"   #'counsel-imenu
 "im"   #'(lambda () (interactive) (sam-counsel-imenu "macros: "))
 "ip"   #'(lambda () (interactive) (sam-counsel-imenu "package "))
 "is"   #'(lambda () (interactive) (sam-counsel-imenu "section: "))
 "iv"   #'(lambda () (interactive) (sam-counsel-imenu "variables: "))
 "it"   #'(lambda () (interactive) (find-file "~/test"))
;;; <leader> i --- imenu
 ;; (:prefix-map ("i" . "imenu")
 ;;              :desc "functions" "f"   #'(lambda () (interactive) (sam-counsel-imenu "functions: "))
 ;;              :desc "default"   "i"   #'counsel-imenu
 ;;              :desc "macros"    "m"   #'(lambda () (interactive) (sam-counsel-imenu "macros: "))
 ;;              :desc "packages"  "p"   #'(lambda () (interactive) (sam-counsel-imenu "package "))
 ;;              :desc "sections"  "s"   #'(lambda () (interactive) (sam-counsel-imenu "section: "))
 ;;              :desc "variables" "v"   #'(lambda () (interactive) (sam-counsel-imenu "variables: "))
 ;;              :desc "test"      "t"   #'(lambda () (interactive) (find-file "~/test")))
 "j"    #'(lambda () (interactive) (avy-goto-word-or-subword-1))

 "k" '(:ignore t :which-key "kill")
 "kb" #'kill-buffer
 "kc" #'kill-buffer-and-window
 "kk" #'kill-this-buffer
 "kw" #'delete-window
 ;; ;;; <leader> k --- kill
 ;;   (:prefix-map ("k" . "kill")
 ;;                :desc "kill buffer"            "b" #'kill-buffer
 ;;                :desc "kill buffer and window" "c" #'kill-buffer-and-window
 ;;                :desc "kill this buffer"       "k" #'kill-this-buffer
 ;;                :desc "kill window"            "w" #'delete-window)

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
 "mb" #'doom-big-font-mode
 "mf" #'flycheck-mode
 "mF" #'toggle-frame-fullscreen
 "mg" #'evil-goggles-mode
 "mi" #'highlight-indent-guides-mode
 "mI" #'doom/toggle-indent-style
 "ml" #'doom/toggle-line-numbers
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
 "p>" #'doom/browse-in-other-project
 "pf" #'projectile-find-file
 "p?" #'doom/find-file-in-other-project
 "p!" #'projectile-run-shell-command-in-root
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
 "px" #'doom/open-project-scratch-buffer
 "pX" #'doom/switch-to-project-scratch-buffer

 "q" '(:ignore t :which-key "quit")
 "qq" #'save-buffers-kill-terminal
 "qQ" #'evil-quit-all-with-error-code
 "qS" #'doom/quicksave-session
 "qL" #'doom/quickload-session
 "qs" #'doom/save-session
 "ql" #'doom/load-session
 "qr" #'doom/restart-and-restore
 "qR" #'doom/restart

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
 ;; "uc" #'sfs-collections
 "ud" #'wordnut-search
 "uk" #'browse-kill-ring
 "ur" #'replace-string
 "uR" #'replace-query
 ;; "us" #'sfs

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
 "zt" #'((lambda () (interactive) (find-file "~/scratch/text/generic.txt")) :which-key "text")
 "zz" #'(doom/open-scratch-buffer                                           :which-key "generic"))

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

;;
;;; Universal evil integration


;; (when (featurep! :editor evil +everywhere)
;;   ;; Have C-u behave similarly to `doom/backward-to-bol-or-indent'.
;;   ;; NOTE SPC u replaces C-u as the universal argument.
;;   (map! :gi "C-u" #'doom/backward-kill-to-bol-and-indent
;;         :gi "C-w" #'backward-kill-word)
;;   ;; Vimmish ex motion keys
;;   ;;make insert work more like emacs


;;   ;; :gi "C-b" #'backward-word
;;   ;; :gi "C-f" #'forward-word)

;;   (after! view
;;     (define-key view-mode-map [escape] #'View-quit-all))
;;   (after! man
;;     (evil-define-key* 'normal Man-mode-map "q" #'kill-current-buffer))

;;   ;; Minibuffer
;;   ;; (define-key! evil-ex-completion-map
;;   ;;   ;; "C-a" #'move-beginning-of-line
;;   ;;   ;; "C-b" #'backward-word
;;   ;;   "C-a"           #'beginning-of-line
;;   ;;   "C-e"           #'end-of-line
;;   ;;   "C-f"           #'forward-char
;;   ;;   "C-b"           #'backward-char
;;   ;;   "M-w"           #'forward-word
;;   ;;   "M-b"           #'backward-word
;;   ;;   "C-s" (if (featurep! :completion ivy)
;;   ;;             #'counsel-minibuffer-history
;;   ;;           #'helm-minibuffer-history))

;;   ;; (define-key! :keymaps +default-minibuffer-maps
;;   ;;   [escape] #'abort-recursive-edit
;;   ;;   "C-v"    #'yank
;;   ;;   "C-z"    (λ! (ignore-errors (call-interactively #'undo)))
;;   ;;   ;; "C-a"    #'move-beginning-of-line
;;   ;;   ;; "C-b"    #'backward-word
;;   ;;   "C-a"           #'beginning-of-line
;;   ;;   "C-e"           #'end-of-line
;;   ;;   "C-f"           #'forward-char
;;   ;;   "C-b"           #'backward-char
;;   ;;   "M-w"           #'forward-word
;;   ;;   "M-b"           #'backward-word
;;   ;;   "C-r"    #'evil-paste-from-register
;;   ;;   ;; "C-n"    #'next-history-element
;;   ;;   ;; "C-p"    #'previous-history-element
;;   ;;   ;; Scrolling lines
;;   ;;   "C-j"    #'next-line
;;   ;;   "C-k"    #'previous-line
;;   ;;   "C-S-j"  #'scroll-up-command
;;   ;;   "C-S-k"  #'scroll-down-command)

;;   (define-key! read-expression-map
;;     "C-j" #'next-line-or-history-element
;;     "C-k" #'previous-line-or-history-element))

(general-define-key
 :keymaps 'help-map
 ;; new keybinds
 "'"    #'describe-char
 "D"    #'doom/help
 "E"    #'doom/sandbox
 "M"    #'doom/describe-active-minor-mode
 "R"    #'doom/reload
 "T"    #'doom/toggle-profiler
 "V"    #'set-variable
 "W"    #'+default/man-or-woman
 "C-k"  #'describe-key-briefly
 "C-l"  #'describe-language-environment
 "C-m"  #'info-emacs-manual

 ;; Unbind `help-for-help'. Conflicts with which-key's help command for the
 ;; <leader> h prefix. It's already on ? and F1 anyway.
 "C-h"  nil

 ;; replacement keybinds
 ;; replaces `info-emacs-manual' b/c it's on C-m now
 "r"    nil
 "rr"   #'doom/reload
 "rt"   #'doom/reload-theme
 "rp"   #'doom/reload-packages
 "rf"   #'doom/reload-font
 "re"   #'doom/reload-env

 ;; replaces `apropos-documentation' b/c `apropos' covers this
 "d"    nil
 "d/"   #'doom/help-search
 "da"   #'doom/help-autodefs
 "db"   #'doom/report-bug
 "dd"   #'doom/toggle-debug-mode
 "df"   #'doom/help-faq
 "dh"   #'doom/help
 "dm"   #'doom/help-modules
 "dn"   #'doom/help-news
 "dN"   #'doom/help-news-search
 "dp"   #'doom/help-packages
 "dP"   #'doom/help-package-homepage
 "dc"   #'doom/help-package-config
 "ds"   #'doom/sandbox
 "dt"   #'doom/toggle-profiler
 "dv"   #'doom/version

 ;; replaces `apropos-command'
 "a"    #'counsel-apropos
 "b"    #'helm-descbinds
 ;; replaces `describe-copying' b/c not useful
 "C-c"  #'describe-coding-system
 "f"    #'counsel-describe-function
 ;; replaces `Info-got-emacs-command-node' b/c redundant w/ `Info-goto-node'
 "F"    #'counsel-describe-face
 "h" #'helpful-at-point
 ;; replaces `describe-language-environment' b/c remapped to C-l
 "L"    #'global-command-log-mode
 ;; replaces `view-emacs-news' b/c it's on C-n too
 "n"    #'doom/help-news
 ;; replaces `finder-by-keyword'
 "o"    #'ace-link-help
 ;; "p"    #'doom/help-packages
 "p"    #'describe-package
 ;; replaces `describe-package' b/c redundant w/ `doom/describe-package'
 "P"    #'find-library
 "v"    #'counsel-describe-variable)
