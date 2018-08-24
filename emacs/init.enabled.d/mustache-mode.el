;;; Mustache Mode
;; [[https://github.com/mustache/emacs]]

(defun $mustache-mode-hook ()
  (emmet-mode 1))

(req-package mustache-mode
  ;; The `auto-mode-alist' setting in `mustache-mode.el' does not have an
  ;; `autoload' directive.
  :mode (("\\.mustache$" . mustache-mode)
         ;; For Hogan.js
         ("\\.\\(hjs\\|hogan\\)$" . mustache-mode))
  :hook (mustache-mode . $mustache-mode-hook))
