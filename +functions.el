;;; +functions.el --- Define personal functions    -*- lexical-binding: t; -*-

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

;; A collection of personal functions.

;;; Code:

(defun empire/haskell/module->test ()
  "Jump from a module to a test."
  (let ((filename (->> buffer-file-name
                       (s-replace "/src/" "/test/")
                       (s-replace ".hs" "Test.hs")
                       find-file)))
    (make-directory (f-dirname filename) t)
    (find-file filename)))

(defun empire/haskell/test->module ()
  "Jump from a test to a module."
  (let ((filename (->> buffer-file-name
                       (s-replace "/test/" "/src/")
                       (s-replace "Test.hs" ".hs")
                       )))
    (make-directory (f-dirname filename) t)
    (find-file filename)))

(defun empire/haskell/test<->module ()
  "Toggle between test and module in Haskell."
  (interactive)
  (if (s-contains? "/src/" buffer-file-name)
      (empire/haskell/module->test)
    (empire/haskell/test->module)))

(defun sam-eval-this-sexp () (interactive)
       (if (eq (char-after) 41)
           (call-interactively 'eval-last-sexp)
         (progn (evil-jump-item)
                (call-interactively 'eval-last-sexp)
                (evil-jump-item))))

(defun sam-insert-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (end-of-line 0)
    (open-line 1)))
(defun sam-follow-newline-above ()
  "Insert an empty line above the current line."
  (interactive)
  (sam-insert-line-above)
  (evil-previous-visual-line))

(defun sam-insert-line-below ()
  "Insert an empty line below the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))
(defun sam-follow-newline-below ()
  "Insert an empty line above the current line."
  (interactive)
  (sam-insert-line-above)
  (evil-next-visual-line))

(defun keychain-refresh-environment ()
  "Set ssh-agent and gpg-agent environment variables.
Set the environment variables `SSH_AUTH_SOCK', `SSH_AGENT_PID'
and `GPG_AGENT' in Emacs' `process-environment' according to
information retrieved from files created by the keychain script."
  (interactive)
  (let* ((ssh (shell-command-to-string "keychain -q --noask --agents ssh --eval"))
         (gpg (shell-command-to-string "keychain -q --noask --agents gpg --eval")))
    (list (and ssh
               (string-match "SSH_AUTH_SOCK[=\s]\\([^\s;\n]*\\)" ssh)
               (setenv       "SSH_AUTH_SOCK" (match-string 1 ssh)))
          (and ssh
               (string-match "SSH_AGENT_PID[=\s]\\([0-9]*\\)?" ssh)
               (setenv       "SSH_AGENT_PID" (match-string 1 ssh)))
          (and gpg
               (string-match "GPG_AGENT_INFO[=\s]\\([^\s;\n]*\\)" gpg)
               (setenv       "GPG_AGENT_INFO" (match-string 1 gpg))))))

(defun neotree-project-root-dir-or-current-dir ()
  "Open NeoTree using the project root, using projectile, or the
current buffer directory."
  (interactive)
  (let ((project-dir (ignore-errors (projectile-project-root)))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
            (neotree-dir project-dir))
        (if file-name
            (neotree-find file-name))))))

(defun +sclang-eval-this-expression () (interactive)
       (if (eq (char-after) 41)
           (call-interactively 'sclang-eval-last-expression)
         (progn (evil-jump-item)
                (call-interactively 'sclang-eval-last-expression)
                (evil-jump-item))))

(defun term-primary-yank ()
  "Paste PRIMARY selection without mouse interaction."
  (interactive)
  (term-send-raw-string (gui-get-primary-selection)))

(setq folds-open t)
(defun toggle-fold-lines ()
  (if folds-open (+fold/close-all) (+fold/open-all))
  (setq folds-open (not folds-open)))

(defun toggle-var (var)
  (setq var (not var)))

(defun wc/buffer-major-mode (buffer-handle)
  "Returns a buffer's active major-mode."
  (with-current-buffer buffer-handle major-mode))

(defun wc/file-buffer-p (buffer-candidate)
  "Returns t if the buffer argument is backed by a file and is therefore presumably a code buffer."
  (interactive)
  (let ((buff-name (buffer-name buffer-candidate))
        (buff-mode (wc/buffer-major-mode buffer-candidate)))
    (not (or (string-match-p "*" buff-name)
             (member buff-mode '(neotree-mode dired-mode))))))

(defun wpc/find-or-create-clojure-or-clojurescript-repl ()
  (interactive)
  (require 'projectile)
  (with-current-buffer (current-buffer)
    (let ((buffer-name   (wpc/buffer-name-for-clojure-mode major-mode))
          (repl-function (wpc/repl-function-for-clojure-mode major-mode)))
      (if (get-buffer buffer-name)
          (switch-to-buffer buffer-name)
        (funcall repl-function)))))

(defun sam-counsel-imenu (initial-input)
  "Jump to a buffer position indexed by imenu."
  (interactive)
  (ivy-read "imenu items: " (counsel--imenu-candidates)
            :initial-input initial-input
            :preselect (thing-at-point 'symbol)
            :require-match t
            :action #'counsel-imenu-action
            :keymap counsel-imenu-map
            :history 'counsel-imenu-history
            :caller 'counsel-imenu))

(defun sam-move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun sam-move-line-up ()
  (interactive)
  (let ((col (current-column)))
    ;; (save-excursion
    (forward-line)
    (transpose-lines -1)
    (previous-line)
    (move-to-column col))
  )

;; Indent relative, but works for the line below rather than above
(defun sam-indent-relative-below ()
  (interactive)
  (move-line-down)
  (indent-relative)
  (move-line-up))

(defun sam-indent-all ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (indent-region (point) (mark))))

(defun consult-line-symbol-at-point ()
  (interactive)
  (consult-line (symbol-name (symbol-at-point))))

(defun consult-line-repeat ()
  (interactive)
  (consult-line (first consult--line-history)))

(defun consult-grep-wd ()
  (interactive)
  (consult-grep default-directory))

(defun rgrep-wd (regexp)
  (interactive
   (list
    (progn
      (grep-compute-defaults)
      (read-regexp "Search regexp: "))))
  (rgrep regexp "\*" (pwd))
  (pop-to-buffer "*grep*"))

(defun sam-replace-string ()
  (interactive)
  (let* ((sap (if mark-active "" (symbol-name (symbol-at-point))))
         (str-orig (read-string "Replace: " sap))
         (str-replace (read-string "With: " sap)))
    (if mark-active
        (replace-string str-orig str-replace nil (point) (mark))
      (replace-string str-orig str-replace nil (point-min) (point-max)))))

;; (defvar-local hs-hidden-p nil)
;; (defun hs-toggle-level ()
;;   (interactive)
;;   (if hs-hidden-p
;;       (progn
;;         (hs-show-all)
;;         (setq-local hs-hidden-p nil))
;;     (progn
;;       (hs-hide-level 1)
;;       (setq-local hs-hidden-p t))))
(defun hs-toggle-level ()
  (interactive)
  (if (hs-already-hidden-p)
      (hs-show-all)
    (hs-hide-level 1)))

(defun sam-drag-sexp-backward ()
  "Drag sexp at POINT backwards."
  (interactive)
  (save-excursion (transpose-sexps 1))
  (sp-backward-sexp))

(defun sam-drag-sexp-forward ()
  "Drag sexp at POINT forwards."
  (interactive)
  (sp-next-sexp)
  (save-excursion
    (transpose-sexps 1)))

(defun sam-next-line-start ()
  (interactive)
  (evil-next-line)
  (back-to-indentation))

(defun sam-previous-line-start ()
  (interactive)
  (evil-previous-line)
  (back-to-indentation))

(defun sam-switch-to-persp-buffer ()
  (interactive)
  (let ((read-buffer-function 'persp-read-buffer))
    (consult-buffer)))

(defun sam-sexp-spawn-below ()
  (interactive)
  (sp-end-of-sexp)
  (sp-split-sexp 0)
  (newline-and-indent)
  (forward-char))

(defun sam-sexp-eject-below ()
  (interactive)
  (sp-end-of-sexp)
  (forward-char)
  (newline-and-indent))

(defun sam-sexp-spawn-right ()
  (interactive)
  (sp-end-of-sexp)
  (sp-split-sexp 0)
  (insert " ")
  (forward-char))

(defun sam-sexp-eject-right ()
  (interactive)
  (sp-end-of-sexp)
  (forward-char)
  (insert " "))

(defun sam-sexp-reparent ()
  (interactive)
  (sp-beginning-of-sexp)
  (backward-char)
  (paredit-wrap-sexp)
  (insert " ")
  (backward-char))

;; (defun sam-insert-at-end-of-form ()
;;   (interactive)
;;   (evil-cp-insert-at-end-of-form)
;;   (insert " "))

(defun sam-insert-at-end-of-form ()
  (interactive)
  (sp-end-of-sexp)
  (evil-insert 1)
  (insert " "))

(defun sam-insert-at-end-of-form ()
  (interactive)
  (let ((line-end (point-at-eol)))
    (when (or (when (sp-up-sexp 1) (backward-char) t)
              (-when-let (enc-end (cdr (evil-cp--top-level-bounds)))
                (goto-char (1- enc-end))))
      (if (<= (point) line-end)
          (progn
            (evil-insert 1)
            (insert " "))
        (insert "\n")
        (indent-according-to-mode)
        (evil-insert 1)))))

(defun xah-delete-backward-char-or-bracket-text ()
  "Delete backward 1 character, but if it's a \"quote\" or bracket ()[]{}【】「」 etc, delete bracket and the inner text, push the deleted text to `kill-ring'.

What char is considered bracket or quote is determined by current syntax table.

If `universal-argument' is called first, do not delete inner text.

URL `http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
  (interactive)
  (if (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end))
    (cond
     ((looking-back "\\s)" 1)
      (if current-prefix-arg
          (xah-delete-backward-bracket-pair)
        (xah-delete-backward-bracket-text)))
     ((looking-back "\\s(" 1)
      (progn
        (backward-char)
        (forward-sexp)
        (if current-prefix-arg
            (xah-delete-backward-bracket-pair)
          (xah-delete-backward-bracket-text))))
     ((looking-back "\\s\"" 1)
      (if (nth 3 (syntax-ppss))
          (progn
            (backward-char )
            (xah-delete-forward-bracket-pairs (not current-prefix-arg)))
        (if current-prefix-arg
            (xah-delete-backward-bracket-pair)
          (xah-delete-backward-bracket-text))))
     (t
      (delete-char -1)))))

(defun xah-delete-backward-bracket-text ()
  "Delete the matching brackets/quotes to the left of cursor, including the inner text.

This command assumes the left of point is a right bracket, and there's a matching one before it.

What char is considered bracket or quote is determined by current syntax table.

URL `http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
  (interactive)
  (progn
    (forward-sexp -1)
    (mark-sexp)
    (kill-region (region-beginning) (region-end))))

(defun xah-delete-backward-bracket-pair ()
  "Delete the matching brackets/quotes to the left of cursor.

After the command, mark is set at the left matching bracket position, so you can `exchange-point-and-mark' to select it.

This command assumes the left of point is a right bracket, and there's a matching one before it.

What char is considered bracket or quote is determined by current syntax table.

URL `http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
  (interactive)
  (let (( $p0 (point)) $p1)
    (forward-sexp -1)
    (setq $p1 (point))
    (goto-char $p0)
    (delete-char -1)
    (goto-char $p1)
    (delete-char 1)
    (push-mark (point) t)
    (goto-char (- $p0 2))))

(defun xah-delete-forward-bracket-pairs ( &optional @delete-inner-text-p)
  "Delete the matching brackets/quotes to the right of cursor.
If *delete-inner-text-p is true, also delete the inner text.

After the command, mark is set at the left matching bracket position, so you can `exchange-point-and-mark' to select it.

This command assumes the char to the right of point is a left bracket or quote, and have a matching one after.

What char is considered bracket or quote is determined by current syntax table.

URL `http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-07-02"
  (interactive)
  (if @delete-inner-text-p
      (progn
        (mark-sexp)
        (kill-region (region-beginning) (region-end)))
    (let (($pt (point)))
      (forward-sexp)
      (delete-char -1)
      (push-mark (point) t)
      (goto-char $pt)
      (delete-char 1))))

(defun sam-lookup-symbol-at-point ()
  (interactive)
  (info-lookup-symbol (symbol-at-point)))

(defun sam-evil-append ()
  (interactive)
  (if (not (equal (point) (line-beginning-position)))
      (call-interactively #'evil-append)
    (call-interactively #'evil-append)
    (indent-according-to-mode)))

(defun sam-helpful-click ()
  (interactive)
  (call-interactively #'mouse-set-point)
  (call-interactively #'helpful-at-point))

(defvar-local sam-last-cursor-type t)
(defvar-local sam-last-hl-line -1)
(defun sam-toggle-cursor ()
  (interactive)
  (if cursor-type
      (progn
        (rotatef sam-last-cursor-type cursor-type)
        (setq-local cursor-type nil)
        (rotatef sam-last-hl-line hl-line-mode)
        (hl-line-mode -1))
    (rotatef cursor-type sam-last-cursor-type)
    (hl-line-mode sam-last-hl-line)))

(defun sam-sudo-find-file ()
  (interactive)
  (find-file (read-file-name "File: " (concat "/su::" default-directory))))

(defun sam-sudo-find-root ()
  (interactive)
  (find-file (read-file-name "File: " "/su::/" )))

(defun sam-find-root ()
  (interactive)
  (find-file (read-file-name "File: " "/" )))

(defun d2h (dec-num)
  (interactive "nNum: ")
  (message (format "%x" dec-num)))

(defun h2d (hex-num)
  (interactive "sNum: ")
  (message "%d" (string-to-number hex-num 16)))

(defvar sam-file-ring (make-ring 10))
(defun sam-dired-kill-path-at-point ()
  (interactive)
  (ring-insert sam-file-ring (dired-file-name-at-point)))

(defun sam-dired-yank-here ()
  (interactive)
  (copy-file (ring-ref sam-file-ring 0) (read-file-name "Target: ")))

(defun sam-create-project (dir)
  "Create projectile project in DIR and spawn perspective."
  (interactive "Ddir: ")
  (require 'perspective)
  (require 'projectile)
  (message dir)
  (projectile-add-known-project dir)
  (message (projectile-project-name dir))
  (persp-switch (projectile-project-name dir))
  (projectile-switch-project-by-name dir)
  (find-file dir)
  (f-touch ".projectile"))

(defun sam-aw-move-window (window)
  "Swap buffers of current window and WINDOW."
  (cl-labels ((swap-windows (window1 window2)
                            "Swap the buffers of WINDOW1 and WINDOW2."
                            (let ((buffer1 (window-buffer window1))
                                  (buffer2 (window-buffer window2)))
                              (set-window-buffer window1 buffer2)
                              (set-window-buffer window2 buffer1)
                              (select-window window2)
                              (delete-window window1))))
    (let ((frame (window-frame window))
          (this-window (selected-window)))
      (when (and (frame-live-p frame)
                 (not (eq frame (selected-frame))))
        (select-frame-set-input-focus (window-frame window)))
      (when (and (window-live-p window)
                 (not (eq window this-window)))
        (aw--push-window this-window)
        (if aw-swap-invert
            (swap-windows window this-window)
          (swap-windows this-window window))))))

(defun sam-ace-move-window ()
  "Ace replace window."
  (interactive)
  (aw-select " Ace - Move Window"
             #'sam-aw-move-window))

(defun sam-open-between ()
  (interactive)
  (evil-open-above 1)
  (newline-and-indent))

(deftoggle sam-hs-toggle-all
  "Toggle showing all top-level blocks"
  (hs-hide-all)
  (hs-show-all))

(deftoggle sam-toggle-theme
  "Toggle theme between light and dark."
  (progn (disable-theme 'dracula)
         (load-theme 'spacemacs-light t))
  (progn (disable-theme 'spacemacs-light)
         (load-theme 'dracula t)))

(defun sam-make-filename-unique (filename)
  (let ((base (file-name-sans-extension filename))
	(ext (file-name-extension filename))
	(name filename)
	(cnt 1))
    (while (file-exists-p name)
      (setf name (concat base "-" (int-to-string cnt) "." ext))
      (1+ cnt))
    name))

(defun sam-take-screenshot ()
  "Take a screenshot and output to screenshot.png"
  (interactive)
  (shell-command "grim"))

(defun sam-yank-to-eol ()
  (interactive)
  (evil-yank-line (point) (line-end-position)))

(defun sam-move-scroll-next-line ()
  (interactive)
  (evil-scroll-line-down 1)
  (evil-next-visual-line))
(defun sam-move-scroll-prev-line ()
  (interactive)
  (evil-scroll-line-up 1)
  (evil-previous-visual-line))

(defun sam-avy ()
  (interactive)
  (avy-goto-word-or-subword-1))

(defun source (filename)
  "Update environment variables from a shell source FILENAME."
  (interactive "fSource file: ")

  (message "Sourcing environment from `%s'..." filename)
  (with-temp-buffer

    (shell-command (format "diff -u <(true; export) <(source %s; export)" filename) '(4))

    (let ((envvar-re "declare -x \\([^=]+\\)=\\(.*\\)$"))
      ;; Remove environment variables
      (while (search-forward-regexp (concat "^-" envvar-re) nil t)
        (let ((var (match-string 1)))
          (message "%s" (prin1-to-string `(setenv ,var nil)))
          (setenv var nil)))

      ;; Update environment variables
      (goto-char (point-min))
      (while (search-forward-regexp (concat "^+" envvar-re) nil t)
        (let ((var (match-string 1))
              (value (read (match-string 2))))
          (message "%s" (prin1-to-string `(setenv ,var ,value)))
          (setenv var value)))))
  (message "Sourcing environment from `%s'... done." filename))

(defun sam-delete-side-windows (side)
  "Delete windows at SIDE."
  (mapc #'(lambda (window) (delete-window window)) (window-at-side-list nil side)))

(defun sam-dired-group-marked (directory)
  "Create a directory called DIRECTORY.
Parent directories of DIRECTORY are created as needed.
If DIRECTORY already exists, signal an error."
  (interactive
   (list (read-file-name "Create directory: " (dired-current-directory))))
  (let* ((expanded (directory-file-name (expand-file-name directory)))
	 (marked (dired-get-marked-files))
	 new)
    (unless (file-exists-p expanded)
      (setq new (dired--find-topmost-parent-dir expanded))
      (make-directory expanded t)
      (when new
	(dired-add-file new)
	(dired-move-to-filename)))
    (mapc #'(lambda (file) (rename-file file (concat expanded "/"))) marked)
    (revert-buffer)))

(defun sam-move-buffer-file (directory)
  "Create a directory called DIRECTORY.
Parent directories of DIRECTORY are created as needed.
If DIRECTORY already exists, signal an error."
  (interactive
   (list (read-file-name "Create directory: " default-directory)))
  (let* ((expanded (directory-file-name (expand-file-name directory)))
	 (filename (buffer-file-name))
	 (new (concat expanded "/" (f-filename (buffer-file-name)))))
    (unless (file-exists-p expanded)
      (make-directory expanded t))
    (rename-file filename new)
    (set-visited-file-name new)
    (get-buffer-create new)
    (set-buffer-modified-p nil)))

(provide '+functions)
;;; +functions.el ends here
