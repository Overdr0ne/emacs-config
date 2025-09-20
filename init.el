;; set to enormous value to prevent gc at start
(setq gc-cons-percentage .9)
(setq gc-cons-threshold most-positive-fixnum)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/overdr0ne")
(setq user-emacs-directory "~/.emacs.d/")
(load "/home/sam/.emacs.d/straight-init.el")
(load "config.el")
;; (error "SAMSAM")

;; (provide 'init)
(put 'dired-find-alternate-file 'disabled nil)
