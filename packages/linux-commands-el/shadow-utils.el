;;; shadow-utils.el --- change user permissions  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Sam

;; Author: Sam <scmorris.dev@gmail.com>
;; Keywords: tools

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

;; 

;;; Code:

(require 'coreutils)

(defun usermod-add-user-to-group (user group)
  (interactive (list (completing-read "User: " (users-real))
                     (completing-read "Groups: " (groups-all))))
  (let* ((default-directory "/sudo::"))
    (shell-command-to-string (concat "usermod -aG " group " " user))))

(defun usermod-add-me-to-group (group)
  (interactive (list (completing-read "Groups: " (groups-all))))
  (let* ((me (whoami))
         (default-directory "/sudo::"))
    (shell-command-to-string (concat "usermod -aG " group " " me))
    (message "Log out and log back in for changes to take effect")))

(provide 'shadow-utils)
;;; shadow-utils.el ends here
