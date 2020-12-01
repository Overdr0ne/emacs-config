;; (load! "+base")
(load "./+functions")
(load "./+core")
(load "./+modules")
(load "./+bindings")
(load "./+langs")

(blink-cursor-mode -1)

(tool-bar-mode -1)

(scroll-bar-mode -1)

(set-frame-parameter (selected-frame) 'alpha '(93 . 70))
(add-to-list 'default-frame-alist '(alpha . (93 . 70)))

(defun +default|disable-delete-selection-mode ()
  (delete-selection-mode -1))
(add-hook 'evil-insert-state-entry-hook #'delete-selection-mode)

(setq org-directory "~/notes")
(setq org-agenda-files '("~/notes"))

(setq tags-add-tables nil)
(setq large-file-warning-threshold nil)
(set-face-attribute 'default nil :height 140)
;; (setq doom-font (font-spec :family "Fira Code" :size 18))
;; (setq doom-font (font-spec :family "Iosevka" :size 18))
(setq display-line-numbers-type nil)
(setq user-mail-address "scmorris.dev@gmail.com")
(turn-off-auto-fill)
(auto-fill-mode -1)

(setq browse-url-browser-function 'eww-browse-url)
;; (add-hook 'eww-mode-hook
;;           (lambda () (load-theme-buffer-local 'plan9 (current-buffer))))

(setq show-trailing-whitespace t)

(setq load-prefer-newer t)

;; (add-hook! (emacs-lisp-mode) #'(lambda () (add-to-list (make-local-variable 'company-backends) 'company-elisp)))
;; (add-hook 'emacs-lisp-mode-hook #'(lambda () (rainbow-delimiters-mode)))


(setq company-idle-delay 10)

(delete "~/.emacs.d/.local/" projectile-globally-ignored-directories)
