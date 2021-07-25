;; (load! "+base")
(load "./+core")
(load "./+functions")
(load "./+modules")
(load "./+bindings")
(load "./+langs")

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq custom-file "~/.emacs.d/overdr0ne/customize.el")
(load custom-file)
;; (general-add-hook 'kill-emacs-hook
;;                   '(customize-save-customized))

;; (setq backup-directory-alist `(("." . "~/.bak")))
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
;; (setq backup-by-copying t)
;; (setq delete-old-versions t
;;   kept-new-versions 6
;;   kept-old-versions 2
;;   version-control t)

(savehist-mode +1)

(blink-cursor-mode -1)

(tool-bar-mode -1)

(scroll-bar-mode -1)

(column-number-mode)

(menu-bar-mode -1)

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
(setq display-line-numbers-type nil)
(setq user-mail-address "scmorris.dev@gmail.com")
(turn-off-auto-fill)
(auto-fill-mode -1)

(setq browse-url-browser-function 'eww-browse-url)

(setq load-prefer-newer t)

(setq inhibit-startup-screen 't)

(delete "~/.emacs.d/.local/" projectile-globally-ignored-directories)

(projectile-mode)
(eval-after-load 'projectile
    (projectile-persp-switch-project (first (projectile-relevant-known-projects))))

(provide 'config)
;;; config.el ends here
