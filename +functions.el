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

(defun sam-eval-this-sexp ()
  (interactive)
  (pulse-momentary-highlight-region (point)
                                    (+ (point)
                                       (length (thing-at-point 'sexp t))))
  (message "%S" (eval (read (thing-at-point 'sexp t)) t)))

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
  (previous-line))

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
  (next-line))

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
    (move-to-column col)))

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
  (consult-line (cl-first consult--line-history)))

(defun sam-ripgrep-wd ()
  (interactive)
  (consult-ripgrep default-directory))

(defun sam-ripgrep-dir (dir)
  (interactive "DDirectory: ")
  (consult-grep dir))

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
(defun hs-toggle-block ()
  (interactive)
  (if (hs-already-hidden-p)
      (hs-show-block)
    (hs-hide-block)))

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
  (next-line)
  (back-to-indentation))

(defun sam-previous-line-start ()
  (interactive)
  (previous-line)
  (back-to-indentation))

(defun sam-switch-to-persp-buffer ()
  (interactive)
  (let ((read-buffer-function 'persp-read-buffer))
    (consult-buffer)))

(defun dired-buffer-p (buffer)
  (if buffer
      (with-current-buffer (car buffer)
        (equal major-mode #'dired-mode))
    nil))

(defun sam-switch-to-dir (dir-buffer)
  (interactive (list
                (read-buffer "Dired buffer: " nil nil #'dired-buffer-p)
                ))
  (switch-to-buffer dir-buffer))

(defun file-list (&optional frame)
  (seq-filter #'stringp (mapcar #'buffer-file-name (buffer-list))))

(defun sam-insert-path (path)
  (interactive (list
                (completing-read "Open files: " (file-list))
                ))
  (insert path))

(defun sam-add-and-switch-to-buffer (buffer)
  (interactive
   (list
    (let ((read-buffer-function nil))
      (read-buffer "Add buffer to this perspective: "))))
  (persp-add-buffer buffer)
  (switch-to-buffer buffer))

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

(defun sam-insert-at-beginning-of-form ()
  (interactive)
  (sp-beginning-of-sexp)
  (insert " ")
  (backward-char)
  (evim-i))

(defun sam-insert-at-end-of-form ()
  (interactive)
  (sp-end-of-sexp)
  (evim-i)
  (insert " "))

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

(defun sam-create-persp-dir (dir)
  "Create perspective in DIR."
  (interactive "Ddir: ")
  (require 'perspective)
  (persp-switch (last (split-string dir "/")))
  (find-file dir))

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
  (evim-open-line-above)
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

(defun sam-scroll-page-up ()
  (interactive)
  (next-line)
  (scroll-up-line)
  )
(defun sam-scroll-page-down ()
  (interactive)
  (previous-line)
  (scroll-down-line)
  )

(defun sam-avy ()
  (interactive)
  (avy-goto-word-or-subword-1))

(defun sam-delete-side-windows (side)
  "Delete windows at SIDE."
  (mapc #'(lambda (window) (delete-window window)) (window-at-side-list nil side)))

(defun sam-delete-window-right-side ()
  (interactive)
  (sam-delete-side-windows 'right))

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

(defun sam-flash-wic ()
  "Flash IMAGE to DEVICE."
  (interactive)
  (with-temp-buffer
    (let* ((image (read-file-name "Image: "))
	       (device (read-file-name "Device: " "/dev/" nil nil "mmcblk")))
      (setq image (expand-file-name image))
      (cd "/sudo::/")
      (shell-command (concat "umount " device "*"))
      (async-shell-command (concat "bmaptool copy " image " " device)))))

(defun sam-flash-ext4 ()
  "Flash IMAGE to DEVICE."
  (interactive)
  (with-temp-buffer
    (let* ((image (read-file-name "Image: "))
	       (device (read-file-name "Device: " "/dev/" nil nil "sd")))
      (setq image (expand-file-name image))
      (cd "/sudo::/")
      (shell-command (concat "umount " device "*"))
      (async-shell-command (concat "bmaptool copy " image " " device)))))

(defun sam-find-file-here ()
  "Find file within current directory."
  (interactive)
  (consult-find default-directory))

(defun sam-serial-read-name ()
  "Read serial device name."
  (read-file-name "Device: " "/dev/" nil t))
(defun sam-serial-read-speed ()
  "Read serial speed from the user."
  (let ((speeds (mapcar #'number-to-string '(110 300 600 1200 2400 4800 9600 14400 19200 38400 57600 115200 128000 256000))))
    (string-to-number (completing-read "Baud: " speeds nil t nil))))
(defun sam-serial-active-buffer (port)
  "Return true if serial PORT is active."
  (cl-dolist (buffer (buffer-list))
    (when (string= (buffer-name buffer)
		           "/dev/ttyUSB0")
      (cl-return buffer))))
(defun sam-serial-term (port speed &optional line-mode)
  "Start a terminal-emulator for a serial port in a new buffer.
PORT is the path or name of the serial port.  For example, this
could be \"/dev/ttyS0\" on Unix.  On Windows, this could be
\"COM1\" or \"\\\\.\\COM10\".

SPEED is the speed of the serial port in bits per second.  9600
is a common value.  SPEED can be nil, see
`serial-process-configure' for details.

Usually `term-char-mode' is used, but if LINE-MODE (the prefix
when used interactively) is non-nil, `term-line-mode' is used
instead.

The buffer is in Term mode; see `term-mode' for the commands to
use in that buffer.

\\<term-raw-map>Type \\[switch-to-buffer] to switch to another buffer."
  (interactive (list (sam-serial-read-name) (sam-serial-read-speed)
                     current-prefix-arg))
  (serial-supported-or-barf)
  (if (sam-serial-active-buffer port)
      (switch-to-buffer (sam-serial-active-buffer port))
    (let* ((process (make-serial-process
                     :port port
                     :speed speed
                     :coding 'no-conversion
                     :noquery t))
           (buffer (process-buffer process)))
      (with-current-buffer buffer
	    (term-mode)
	    (unless line-mode
          (term-char-mode))
	    (goto-char (point-max))
	    (set-marker (process-mark process) (point))
	    (set-process-filter process #'term-emulate-terminal)
	    (set-process-sentinel process #'term-sentinel))
      (switch-to-buffer buffer)
      buffer)))

(defun sam-read-extended-command (initial-input)
  "Read command name to invoke in `execute-extended-command' with INITIAL-INPUT."
  (minibuffer-with-setup-hook
      (lambda ()
        (add-hook 'post-self-insert-hook
                  (lambda ()
                    (setq execute-extended-command--last-typed
                          (minibuffer-contents)))
                  nil 'local)
	    (set (make-local-variable 'minibuffer-default-add-function)
	         (lambda ()
	           ;; Get a command name at point in the original buffer
	           ;; to propose it after M-n.
	           (with-current-buffer (window-buffer (minibuffer-selected-window))
		         (and (commandp (function-called-at-point))
		              (format "%S" (function-called-at-point)))))))
    ;; Read a string, completing from and restricting to the set of
    ;; all defined commands.  Don't provide any initial input.
    ;; Save the command read on the extended-command history list.
    (completing-read
     (concat (cond
	          ((eq current-prefix-arg '-) "- ")
	          ((and (consp current-prefix-arg)
		            (eq (car current-prefix-arg) 4)) "C-u ")
	          ((and (consp current-prefix-arg)
		            (integerp (car current-prefix-arg)))
	           (format "%d " (car current-prefix-arg)))
	          ((integerp current-prefix-arg)
	           (format "%d " current-prefix-arg)))
	         ;; This isn't strictly correct if `execute-extended-command'
	         ;; is bound to anything else (e.g. [menu]).
	         ;; It could use (key-description (this-single-command-keys)),
	         ;; but actually a prompt other than "M-x" would be confusing,
	         ;; because "M-x" is a well-known prompt to read a command
	         ;; and it serves as a shorthand for "Extended command: ".
	         "M-x ")
     (lambda (string pred action)
       (let ((pred
              (if (memq action '(nil t))
                  ;; Exclude obsolete commands from completions.
                  (lambda (sym)
                    (and (funcall pred sym)
                         (or (equal string (symbol-name sym))
                             (not (get sym 'byte-obsolete-info)))))
                pred)))
         (complete-with-action action obarray string pred)))
     #'commandp t initial-input 'extended-command-history)))

(defun sam-bitbake ()
  "Read command with bitbake as prefix."
  (interactive)
  (let ((cmd (intern-soft (sam-read-extended-command "bitbake- "))))
    (command-execute cmd 'record)))

(defun sam-read-tsv ()
  "Read a IMAGE in the minibuffer, with completion."
  (read-file-name "TSV: "
		          "."
		          nil
		          nil
		          nil
		          (lambda (filename) (or (string-match-p "tsv" filename)
					                     (string-match-p "/" filename)))))
(defun sam-stm32-program (tsv)
  "Run stm32 programmer with TSV."
  (interactive (list (sam-read-tsv)))
  (let ((cmd "/home/sam/apps/stm32programmer/bin/STM32_Programmer_CLI")
	    (port "usb1"))

    (setq tsv (expand-file-name tsv))
    ;; (cd "/sudo::/")
    (message tsv)
    (async-shell-command (concat "sudo "
				                 cmd
				                 " -c "
				                 " port=" port
				                 " -w "
				                 tsv))
    (cd default-directory)))

(defun sam-pushb-or-embark (pos &optional event)
  "Invoke button at POS, or call embark-act."
  (interactive "@d")
  (let ((button (get-char-property pos 'button)))
    ;; If there is no button at point, then use the one at the start
    ;; of the line, if it is a custom-group-link (bug#2298).
    (if button
	    (push-button pos)
      ;; (widget-apply-action button event)
      (call-interactively #'sam-embark-act))))

(defun sam-pushw-or-embark (pos &optional event)
  "Invoke button at POS, or call embark-act."
  (interactive "@d")
  (let ((button (get-char-property pos 'button)))
    ;; If there is no button at point, then use the one at the start
    ;; of the line, if it is a custom-group-link (bug#2298).
    (if button
	    (widget-button-press pos)
      ;; (widget-apply-action button event)
      (call-interactively #'embark-act))))

(defun sam-bookmark (&optional arg)
  "Run the default action on the current target.
The target of the action is chosen by `embark-target-finders'.

If the target comes from minibuffer completion, then the default
action is the command that opened the minibuffer in the first
place, unless overidden by `embark-default-action-overrides'.

For targets that do not come from minibuffer completion
\(typically some thing at point in a regular buffer) and whose
type is not listed in `embark-default-action-overrides', the
default action is given by whatever binding RET has in the action
keymap for the target's type.

See `embark-act' for the meaning of the prefix ARG."
  (interactive "P")
  (if-let ((targets (embark--targets)))
      (let* ((target
              (or (nth
                   (if (or (null arg) (minibufferp))
                       0
                     (mod (prefix-numeric-value arg) (length targets)))
                   targets)))
             (default-action (embark--default-action (plist-get target :type)))
             (action (or (command-remapping default-action) default-action)))
        (when (and arg (minibufferp)) (setq embark--toggle-quit t))
        (embark--act action
                     (if (and (eq default-action embark--command)
                              (not (memq default-action
                                         embark-multitarget-actions)))
                         (embark--orig-target target)
                       target)
                     (embark--quit-p action)))
    (user-error "No target found")))

(defun sam-impinj-flash ()
  (interactive)
  (let ()
    (shell-command "ssh impinj-rpi \"i2cset -f -y 1 0x18 2\"")
    (shell-command "swpwrctl.sh 10.102.3.2 4 0")
    (shell-command "swpwrctl.sh 10.102.3.2 4 1")
    (shell-command "ssh impinj-rpi \"sudo /home/ubuntu/utils/imx_usb_loader/imx_usb /home/ubuntu/files/u-boot-dtb.imx\""))
  )

(defvar bitbake-deploy-dir "~/workspaces/")
(defvar itb-target "/ssh:impinj-rpi:/tftpboot/")
(defun sam-flash-itb ()
  (interactive)
  (let ((itb-path (read-file-name "itb: " (expand-file-name bitbake-deploy-dir) "" t nil
				                  (lambda (filename) (string-match-p "\.itb" filename))))
	    (rpi-tftpboot (expand-file-name itb-target)))
    (copy-file itb-path rpi-tftpboot t)))

(setq itb-target "ubuntu@10.102.3.1:/tftpboot")
(defun sam-impinj-flash-itb ()
  (interactive)
  (let* ((itb-path (read-file-name "itb: " (expand-file-name bitbake-deploy-dir) "" t nil
				                   (lambda (filename) (string-match-p "\.itb" filename))))
	     (rpi-tftpboot itb-target)
         (cmd (format "rsync -chazvP --progress --stats %s %s" itb-path rpi-tftpboot)))
    (message cmd)
    (shell-command cmd)))

(defun sam-impinj-flash-imx ()
  (interactive)
  (let* ((src (read-file-name "imx: " (expand-file-name "~/workspaces/impinj/container/build/build/tmp/deploy/images/r700/u-boot-impinj/") "" t nil
			                 (lambda (filename) (or (string-match-p "\.imx" filename)
						                            (string-match-p "/" filename)))))
	    (tar "impinj-rpi:/home/ubuntu/files/sam/")
        (cmd (format "rsync -chazvP --progress --stats %s %s" src tar)))
    (message cmd)
    (shell-command cmd)
    ;; (copy-file src tar t)
    ))

(defun sam-rsync (src tar)
  (interactive "fSource: \nfTarget: ")
  (let* ((cmd (format "rsync -chazvP --progress --stats %s %s" src tar)))
    (message cmd)
    (shell-command cmd))
  )

(defun sam-toggle-var (var-name)
  "Toggle VAR."
  (interactive (list (completing-read "Variable: " obarray)))
  (set (intern var-name) (not (eval (intern var-name))))
  (message "Variable %s set to %S." var-name (eval (intern var-name))))

(defun sam-projectile-find-root ()
  (interactive)
  (find-file (projectile-project-root)))

(defun sam-buffer-reload ()
  (interactive)
  (find-file (buffer-file-name)))

(defun sam-minibuffer-history ()
  (interactive)
  (when-let (selection (completing-read "History: " (minibuffer-history-value)))
    (kill-region (minibuffer-prompt-end) (line-end-position))
    (insert selection)))

(defun sam-project-persp-switch-project (dir)
  "\"Switch\" to another project by running an Emacs command.
The available commands are presented as a dispatch menu
made from `project-switch-commands'.

When called in a program, it will use the project corresponding
to directory DIR."
  (interactive (list (project-prompt-project-dir)))
  (message "SAMSAM: %s" dir)
  (project-remember-project (project--find-in-directory dir))
  (persp-switch (car (last (split-string (directory-file-name dir) "/"))))
  (let ((command (if (symbolp project-switch-commands)
                     project-switch-commands
                   (project--switch-project-command))))
    (let ((default-directory dir)
          (project-current-inhibit-prompt t))
      (call-interactively command))))

(defun sam-comment-line ()
  (interactive)
  (save-excursion (comment-line 1)))

(defun sam-ansi-term (program &optional new-buffer-name)
  "Start a terminal-emulator in a new buffer.
This is almost the same as `term' apart from always creating a new buffer,
and `C-x' being marked as a `term-escape-char'."
  (interactive (list (read-from-minibuffer "Run program: "
					                       (or explicit-shell-file-name
					                           (getenv "ESHELL")
					                           shell-file-name))))

  ;; Pick the name of the new buffer.
  (setq term-ansi-buffer-name
	    (if new-buffer-name
	        new-buffer-name
	      (if term-ansi-buffer-base-name
	          (if (eq term-ansi-buffer-base-name t)
		          (file-name-nondirectory program)
		        term-ansi-buffer-base-name)
	        "ansi-term")))

  (setq term-ansi-buffer-name (concat "*" term-ansi-buffer-name "*"))

  ;; In order to have more than one term active at a time
  ;; I'd like to have the term names have the *term-ansi-term<?>* form,
  ;; for now they have the *term-ansi-term*<?> form but we'll see...

  (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
  (setq term-ansi-buffer-name (term-ansi-make-term term-ansi-buffer-name program))

  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-char-mode)

  ;; Historical baggage.  A call to term-set-escape-char used to not
  ;; undo any previous call to t-s-e-c.  Because of this, ansi-term
  ;; ended up with both C-x and C-c as escape chars.  Who knows what
  ;; the original intention was, but people could have become used to
  ;; either.   (Bug#12842)
  (let (term-escape-char)
    ;; I wanna have find-file on C-x C-f -mm
    ;; your mileage may definitely vary, maybe it's better to put this in your
    ;; .emacs ...
    (term-set-escape-char ?\C-x))

  (pop-to-buffer term-ansi-buffer-name))

(defmacro defcommand (name &optional docstring &rest body)
  `(let (())
     (defun ,(intern name)
         (interactive)
       ,body)))

(defun sam-match ()
  "Go to matching parenthesis under cursor."
  (interactive)
  (let ((c (char-after)))
    (cond
     ((equal c 40)
      (forward-list)
      (backward-char))
     ((equal c 41)
      (forward-char)
      (backward-list))
     (t (message "No matchable character under cursor.")))))

(defun project-setup-impinj ()
  (interactive)
  (project-switch-project "~/workspaces/impinj/container/")
  (split-window-right)
  (windmove-right)
  (evim--term "zsh"))

(defun sam-column-at (point)
  "Return column number at POINT."
  (save-excursion
    (goto-char point)
    (current-column)))

(defun sam-scroll-up-command (&optional arg)
  "Scroll text of selected window upward ARG lines; or near full screen if no ARG.
If `scroll-error-top-bottom' is non-nil and `scroll-up' cannot
scroll window further, move cursor to the bottom line.
When point is already on that position, then signal an error.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll downward.
If ARG is the atom `-', scroll downward by nearly full screen."
  (interactive "^P")
  (let* ((top-line (save-excursion (move-to-window-line 0)
                                   (line-number-at-pos)))
         (rel-line (- (line-number-at-pos) top-line))
         (col (sam-column-at (point))))
    (cond
     ((null scroll-error-top-bottom)
      (scroll-up arg))
     ((eq arg '-)
      (scroll-down-command nil))
     ((< (prefix-numeric-value arg) 0)
      (scroll-down-command (- (prefix-numeric-value arg))))
     ((eobp)
      (scroll-up arg))                   ; signal error
     (t
      (condition-case nil
	      (scroll-up arg)
        (end-of-buffer
         (if arg
	         ;; When scrolling by ARG lines can't be done,
	         ;; move by ARG lines instead.
	         (forward-line arg)
	       ;; When ARG is nil for full-screen scrolling,
	       ;; move to the bottom of the buffer.
	       (goto-char (point-max)))))))
    (setq top-line (save-excursion (move-to-window-line 0)
                                   (line-number-at-pos)))
    (goto-line (+ rel-line top-line))
    (move-to-column col))
  )

(defun sam-scroll-down-command (&optional arg)
  "Scroll text of selected window down ARG lines; or near full screen if no ARG.
If `scroll-error-top-bottom' is non-nil and `scroll-down' cannot
scroll window further, move cursor to the top line.
When point is already on that position, then signal an error.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll upward.
If ARG is the atom `-', scroll upward by nearly full screen."
  (interactive "^P")
  (let* ((top-line (save-excursion (move-to-window-line 0)
                                   (line-number-at-pos)))
         (rel-line (- (line-number-at-pos) top-line))
         (col (sam-column-at (point))))
    (cond
     ((null scroll-error-top-bottom)
      (scroll-down arg))
     ((eq arg '-)
      (scroll-up-command nil))
     ((< (prefix-numeric-value arg) 0)
      (scroll-up-command (- (prefix-numeric-value arg))))
     ((bobp)
      (scroll-down arg))                 ; signal error
     (t
      (condition-case nil
	      (scroll-down arg)
        (beginning-of-buffer
         (if arg
	         ;; When scrolling by ARG lines can't be done,
	         ;; move by ARG lines instead.
	         (forward-line (- arg))
	       ;; When ARG is nil for full-screen scrolling,
	       ;; move to the top of the buffer.
	       (goto-char (point-min)))))))
    (setq top-line (save-excursion (move-to-window-line 0)
                                   (line-number-at-pos)))
    (goto-line (+ rel-line top-line))
    (move-to-column col)))

(defun sam-embark-act ()
  "Prompt the user for an action and perform it.
The targets of the action are chosen by `embark-target-finders'.
By default, if called from a minibuffer the target is the top
completion candidate.  When called from a non-minibuffer buffer
there can multiple targets and you can cycle among them by using
`embark-cycle' (which is bound by default to the same key
binding `embark-act' is, but see `embark-cycle-key').

This command uses `embark-prompter' to ask the user to specify an
action, and calls it injecting the target at the first minibuffer
prompt.

If you call this from the minibuffer, it can optionally quit the
minibuffer.  The variable `embark-quit-after-action' controls
whether calling `embark-act' with nil ARG quits the minibuffer,
and if ARG is non-nil it will do the opposite.  Interactively,
ARG is the prefix argument.

If instead you call this from outside the minibuffer, the first
ARG targets are skipped over (if ARG is negative the skipping is
done by cycling backwards) and cycling starts from the following
target."
  (interactive)
  (let* ((targets (embark--targets))
         (selects (mapcar (lambda (target)
                            (plist-get target :target))
                          targets))
         (targets-alist (mapcar (lambda (target)
                                  `(,(substring-no-properties (plist-get target :target)) . ,target) )
                                targets))
         (indicators (mapcar #'funcall embark-indicators))
         (default-done nil))
    (unwind-protect
        (while
            (let* ((select (completing-read "Embark target: "
                                            selects))
                   (target (alist-get select
                                      targets-alist nil nil 'equal))
                   (action
                    (or (embark--prompt
                         indicators
                         (let ((embark-default-action-overrides
                                (if default-done
                                    `((t . ,default-done))
                                  embark-default-action-overrides)))
                           (embark--action-keymap (plist-get target :type)
                                                  (cdr targets)))
                         targets)
                        (user-error "Canceled")))
                   (default-action (or default-done
                                       (embark--default-action
                                        (plist-get target :type)))))
              (cond
               ;; When acting twice in the minibuffer, do not restart
               ;; `embark-act'.  Otherwise the next `embark-act' will
               ;; find a target in the original buffer.
               ((eq action #'embark-act)
                (message "Press an action key"))
               ((eq action #'embark-cycle)
                (setq targets (embark--rotate
                               targets (prefix-numeric-value prefix-arg))))
               (t
                ;; if the action is non-repeatable, cleanup indicator now
                (let ((repeat (embark--action-repeatable-p action)))
                  (unless repeat (mapc #'funcall indicators))
                  (condition-case err
                      (embark--act
                       action
                       (if (and (eq action default-action)
                                (eq action embark--command)
                                (not (memq action embark-multitarget-actions)))
                           (embark--orig-target target)
                         target)
                       (embark--quit-p action))
                    (user-error
                     (funcall (if repeat #'message #'user-error)
                              "%s" (cadr err))))
                  (when-let (new-targets (and repeat (embark--targets)))
                    ;; Terminate repeated prompter on default action,
                    ;; when repeating. Jump to the region type if the
                    ;; region is active after the action, or else to the
                    ;; current type again.
                    (setq default-done #'embark-done
                          targets
                          (embark--rotate
                           new-targets
                           (or (cl-position-if
                                (let ((desired-type
                                       (if (eq repeat t)
                                           (plist-get (car targets) :type)
                                         repeat)))
                                  (lambda (x)
                                    (eq (plist-get x :type) desired-type)))
                                new-targets)
                               0)))))))))
      (mapc #'funcall indicators))))

(defun sshfs (src tar)
  (interactive "FRemote source: \nDLocal target: ")
  (async-shell-command (format "sudo sshfs -o allow_other,default_permissions %s %s" src tar))
  )
(defun rsshfs (src user addr tar)
  (interactive "DLocal source: \nsRemote user: \nsRemote addr: \nsRemote target: ")
  (async-shell-command (format "rsshfs %s %s %s 10000 %s" src user addr tar))
  )
(provide '+functions)
;;; +functions.el ends here
