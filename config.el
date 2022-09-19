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
;; bullshit fixes for keyword symbols being interpreted as variables
(setq :narrow ':narrow)
(setq :state ':state)
(setq :v-adjust ':v-adjust)
(setq :page-nums ':page-nums)
(setq :curr-line ':curr-line)
(setq :annotate ':annotate)
(setq :add-history ':add-history)
(setq :lookup ':lookup)
(setq :debounce ':debounce)
(setq :actions ':actions)
(setq :cl ':cl)
(setq :skip-match ':skip-match)
(setq :global-skip ':global-skip)
(setq :translator ':translator)
(setq :point-marker ':point-marker)
(setq :bound ':bound)
(setq :unset ':unset)
(setq :beg-in ':beg-in)

(require 'straight)
(setq straight-disable-byte-compilation t)
(setq straight-disable-native-compilation t)
(require '+macros)
(require '+functions)
(require '+core)
(require '+modules)
;; (require '+bindings)
(require '+bindings)
(require '+langs)
;; (require 'new-langs)
(load custom-file)
(require '+splash)

(provide 'config)
;;; config.el ends here
