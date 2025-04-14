;;; disambiguate-keys.el --- disambiguate key combinations from their serial counterparts  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Sam

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

;; Some key combinations are interpreted as the same keycode by the command
;; loop because that is how serial terminals originally defined those keys.
;; This package disambiguates those key combinations by modifying the input
;; decode map to wrap those key sequences in angle brackets.

;;; Code:

(define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
;; (define-key input-decode-map [?\C-m] (kbd "<C-m>"))
;; (define-key input-decode-map [? ] (kbd "SPC"))
;; (define-key input-decode-map [?\C-j] (kbd "C-j"))

(provide '+disambiguate-keys)
;;; disambiguate-keys.el ends here
