;; Windows
;; Fix annoying vertical window splitting.
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2015-08/msg00339.html
(with-eval-after-load "window"
  (setq max-window-width (window-width))
  (defcustom split-window-below nil
    "If non-nil, vertical splits produce new windows below."
    :group 'windows
    :type 'boolean)

  (defcustom split-window-right nil
    "If non-nil, horizontal splits produce new windows to the right."
    :group 'windows
    :type 'boolean)

  (fmakunbound #'split-window-sensibly)

  (defun split-window-sensibly
      (&optional window)
    (setq window (or window (selected-window)))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (split-window window nil (if split-window-right 'left  'right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (split-window window nil (if split-window-below 'above 'below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding the
             ;; value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (split-window window nil (if split-window-right
                                              'left
                                            'right))))))))

(setq-default split-height-threshold  4
              split-width-threshold   159)

(setq
 ;; Maximum number of side-windows to create on (left top right bottom)
 window-sides-slots '(1 1 1 1)
 ;; Default rules
 display-buffer-alist
 `(;; Display *Help* buffer at the bottom-most slot
   ("\\(*\\(info\\|Help\\|help\\|helpful\\|trace-\\|Backtrace\\|RefTeX.*\\)\\)"
    (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
    (side . right)
    (slot . 0)
    (window-width . 80)
    (reusable-frames . visible))
   ("\\(magit-diff\\)"
    (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-pop-up-window)
    (pop-up-frame-parameters
     (width . 80)
     (left . 1.0)
     (fullscreen . fullheight)))
   ("\\(\\*draft\\*\\|Draft/\\)"
    (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-pop-up-frame)
    (pop-up-frame-parameters
     (width . 80)
     (left . 1.0)
     (fullscreen . fullheight)))
   ;; Display *BBDB* buffer on the bottom frame
   ("\\*BBDB"
    (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
    (side . bottom)
    (slot . 0)
    (window-height . 10)
    (reusable-frames . visible))
   ;; Split shells at the bottom
   ("^\\*e?shell"
    (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-below-selected)
    (window-min-height . 20)
    (reusable-frames . visible)
    )
   ("magit"
    (display-buffer-reuse-window display-buffer-same-window)))
 )

