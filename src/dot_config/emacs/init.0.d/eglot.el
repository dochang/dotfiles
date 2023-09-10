(defun $eglot-before-save-hook ()
  (require 'eglot)
  ;; `eglot-managed-p' is not autoloaded.  Require `eglot' first.
  (when (eglot-managed-p)
    (eglot-format-buffer)))

(define-advice eglot-ensure (:before-while () lookup-before-ensure)
  (and buffer-file-name (eglot--lookup-mode major-mode)))

(setup (:package eglot)

  (:with-mode (prog-mode conf-mode text-mode)
    (:hook eglot-ensure))

  (add-hook 'before-save-hook '$eglot-before-save-hook)

  )
