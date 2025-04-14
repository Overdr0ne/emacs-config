;;; config.el --- Personal config.      -*- lexical-binding: t; -*-

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

;; Top-level of my personal config.

;;; Code:
;;(require 'straight)
;; (setq straight-disable-byte-compilation t)
;; (setq straight-disable-native-compilation t)

(require '+disambiguate-keys)
(require '+macros)
(require '+core)
(require '+modules)
(require '+functions)
(require '+bindings)
(require '+langs)
(load custom-file)
(require '+modes)
(require '+splash)
(require '+late)


(provide 'config)
;;; config.el ends here
