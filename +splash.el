;;; +splash.el --- Splash screen on startup.    -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Sam

;; Author: Sam
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

;; Make a start page.

;;; Code:

(require '+modules)
(require 'server)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (run-with-idle-timer 0.5 nil
                     (lambda ()
                       ;; (require '+disambiguate-keys)
                       ;; (call-interactively #'sam-project-persp-switch-project)
                       ))
            ))

;; (set-frame-font sam-font nil t)
;;(windmove-display-same-window)
;; (sfs-recollect)
;; (run-with-idle-timer 1.5 nil
;;                      (lambda ()
;;                        (set-frame-font sam-font nil t)
;;                        ;; (require '+disambiguate-keys)
;;                        ;; (persp-mode +1)
;;                        (call-interactively #'sam-project-persp-switch-project)
;;                        ))

(provide '+splash)
;;; +splash.el ends here
