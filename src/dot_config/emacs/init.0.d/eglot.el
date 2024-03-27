(defun $eglot-before-save-hook ()
  (require 'eglot)
  ;; `eglot-managed-p' is not autoloaded.  Require `eglot' first.
  (when (eglot-managed-p)
    (eglot-format-buffer)))

(define-advice eglot-ensure (:before-while () lookup-before-ensure)
  (and buffer-file-name (eglot--lookup-mode major-mode)))

(define-advice eglot--languageId (:override (&optional server &rest args) fix-json-modes)
  "Because these json modes are derived from `js-mode', so return
nil if `major-mode' is a json mode and `mode' is a js mode."
  (setq server (or server (eglot--current-server-or-lose)))
  (cl-loop for (mode . languageid) in
           (eglot--languages server)
           when (cond ((and (provided-mode-derived-p major-mode 'js-json-mode 'json-mode)
                            (provided-mode-derived-p mode 'js-mode 'javascript-mode)
                            (not (provided-mode-derived-p mode 'js-json-mode 'json-mode)))
                       nil)
                      ;; Because these json modes are derived from `js-mode',
                      ;; so return nil if `major-mode' is a json mode and
                      ;; `mode' is a js mode.
                      (t
                       (provided-mode-derived-p major-mode mode)))
           return languageid))

(setup (:package eglot)

  (:with-mode (prog-mode conf-mode text-mode)
    (:hook eglot-ensure))

  (add-hook 'before-save-hook '$eglot-before-save-hook)

  (:when-loaded

    (setq eglot-server-programs
          (append '((kotlin-ts-mode "kotlin-language-server")
                    ((toml-mode toml-ts-mode)
                     . ("taplo" "lsp" "stdio")))
                  eglot-server-programs))

    )

  )
