(setup (:package lisp-local)

  (:with-mode (emacs-lisp-mode lisp-mode scheme-mode clojure-mode)
    (:hook lisp-local))
  ;; `lisp-local' does not support `lisp-data-mode'.
  ;;
  ;; See `lisp-local--indent-properties'.
  ;;
  ;; https://github.com/lispunion/emacs-lisp-local/blob/22e221c9330d2b5dc07e8b2caa34c83ac7c20b0d/lisp-local.el#L120-L129

  )
