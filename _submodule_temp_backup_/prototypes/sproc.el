(defun compose-plist (pstring)
  )

(defun sam-plist ()
  (split-string (shell-command-to-string "ps x -o comm=") "[\f\n]+" t))

(defcustom sproc-slot-schema '(name pid args duration)
  "Entry slot order for perusing the backlog."
  :type 'list)

(defcustom sproc-separator "|"
  "Separator to be used between gumshoe slots."
  :type 'string)

(defface sproc-separator-face
  '((t
     :inherit diary))
  "Face for peruse separators.")

(defclass sproc--entry ()
  ((name :initform ""
         :documentation "The full path of this entry.")
   (pid :initform -1
        :documentation "The buffer object of this entry."
        :printer buffer-file-name)
   (args :initform (point)
         :documentation "Buffer position of this entry.")
   (duration :initform 0
             :documentation "Indicates the date and time of this entry.")
   )
  "Entry class for Gumshoe’s backlog.")

;;; Peruse
(defun sproc--format-record (rec format-string slot-spec)
  "Format REC according to FORMAT-STRING using SLOT-SPEC fields."
  (let* ((slot-vals (mapcar #'(lambda (slot)
				                        (ignore-error invalid-slot-name
				                          (slot-value rec slot))) slot-spec)))
    (apply #'format format-string slot-vals)))
(defun sproc--format-records (rec-list format-string slot-spec)
  "Format records in REC-LIST according to FORMAT-STRING using SLOT-SPEC fields."
  (mapcar #'(lambda (rec) (gumshoe--format-record rec format-string slot-spec)) rec-list))
(defun sproc--dispatch (cmd recs slot-spec &optional entry-filter)
  "Peruse SLOT-SPEC fields of RECS.

Pre-filter results with ENTRY-FILTER."
  (let* ((entries recs)
         (format-schema (string-join (mapcar #'symbol-name slot-spec) (propertize sproc-separator 'face 'sproc-separator-face)))
         (prompt (concat (propertize "(" 'face 'sproc-separator-face)
			                   format-schema
			                   (propertize ")" 'face 'sproc-separator-face) ": "))
         (format-components (mapcar #'(lambda (_) "%s") slot-spec))
	       (separator (propertize sproc-separator 'face 'sproc-separator-face))
         (format-string (string-join format-components separator))
         (filtered-entries (if entry-filter
                               (seq-filter entry-filter entries)
                             entries))
         (entry-strings (gumshoe--format-records filtered-entries format-string slot-spec))
         (candidates (cl-mapcar #'list entry-strings filtered-entries))
         (candidate (completing-read prompt candidates)))
    (call-interactively cmd (cadr (assoc candidate candidates)))))

