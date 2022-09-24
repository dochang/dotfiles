;;; Mustache Mode
;; [[https://github.com/mustache/emacs]]

(req-package mustache-mode
  ;; The `auto-mode-alist' setting in `mustache-mode.el' does not have an
  ;; `autoload' directive.
  :mode (("\\.mustache\\'" . mustache-mode)
         ;; For Hogan.js
         ("\\.\\(hjs\\|hogan\\)\\'" . mustache-mode)))
