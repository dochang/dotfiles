(req-package helpful
  :bind (("C-h f" . helpful-callable)
         ;; Note that the built-in `describe-function' includes both functions
         ;; and macros. `helpful-function' is functions only, so we provide
         ;; `helpful-callable' as a drop-in replacement.
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ;; Lookup the current symbol at point. C-c C-d is a common keybinding
         ;; for this in lisp modes.
         ("C-h F" . helpful-function)
         ;; Look up *F*unctions (excludes macros).
         ;;
         ;; By default, C-h F is bound to
         ;; `Info-goto-emacs-command-node'. Helpful already links to the
         ;; manual, if a function is referenced there.
         ("C-h C" . helpful-command)
         ;; Look up *C*ommands.
         ;;
         ;; By default, C-h C is bound to describe `describe-coding-system'. I
         ;; don't find this very useful, but it's frequently useful to only
         ;; look at interactive functions.
         ))

