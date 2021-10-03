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

(require 'projectile)

;; (projectile-persp-switch-project (first (projectile-relevant-known-projects)))
;; (windmove-display-same-window)
(sfs-recollect)
(call-interactively #'projectile-persp-switch-project)

(provide '+splash)
;;; +splash.el ends here
