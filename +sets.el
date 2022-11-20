(defvar +repl-maps)
(setq +repl-maps '(bitbake-minor-mode-map term-mode-map shell-mode-map term-raw-map geiser-repl-mode-map))

(defvar +evim-maps)
(setq +evim-maps '(evim-normal-mode-map evim-insert-mode-map evim-visual-mode-map))

(defvar +minibuffer-maps nil
  "A list of all the keymaps used for the minibuffer.")
(setq +minibuffer-maps
      `(minibuffer-mode-map
        minibuffer-local-map
        minibuffer-local-ns-map
        minibuffer-local-isearch-map
        minibuffer-inactive-mode-map
        read-expression-map))

(defvar +all-maps '(special-mode-map text-mode-map prog-mode-map))
(setq +all-maps
      (cl-remove-duplicates
       (append +all-maps +repl-maps +evim-maps +minibuffer-maps
               '(Info-mode-map diff-mode-map dired-mode-map bitbake-minor-mode-map term-mode-map debugger-mode-map helpful-mode-map))))

(provide '+sets)
