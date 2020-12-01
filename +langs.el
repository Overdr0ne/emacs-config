(setq auto-insert-mode 't)

;;; prog mode
(general-add-hook '(closure-mode-hook elisp-mode-hook emacs-lisp-mode-hook lisp-mode-hook)
                  '(show-paren-mode visual-line-mode rainbow-delimiters-mode highlight-defined-mode
				    electric-pair-mode electric-quote-mode electric-layout-mode))

;;; lisps
(general-add-hook '(closure-mode-hook elisp-mode-hook emacs-lisp-mode-hook lisp-mode-hook)
                  '(evil-cleverparens-mode))
;; (general-add-hook '(closure-mode-hook emacs-lisp-mode-hook lisp-mode-hook) #'rainbow-delimiters-mode)
;; (general-add-hook '(closure-mode-hook emacs-lisp-mode-hook lisp-mode-hook) #'visual-line-mode)
;; (general-add-hook '(closure-mode-hook emacs-lisp-mode-hook lisp-mode-hook) #'evil-cleverparens-mode)
;; (general-add-hook '(closure-mode-hook emacs-lisp-mode-hook lisp-mode-hook) #'smartparens-mode)
;; (general-add-hook '(closure-mode-hook emacs-lisp-mode-hook lisp-mode-hook) #'(lambda () (setq indent-tabs-mode nil)))

;;; text
(remove-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'visual-line-mode)

;;; markdown
(remove-hook 'markdown-mode-hook 'auto-fill-mode)
