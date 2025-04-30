(defun $eglot-before-save-hook ()
  (require 'eglot)
  ;; `eglot-managed-p' is not autoloaded.  Require `eglot' first.
  (when (eglot-managed-p)
    (eglot-code-action-organize-imports (point-min) (point-max))
    ;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md#organizing-imports-with-eglot
    (eglot-format-buffer)))

;; Run `eglot-ensure' only if the mode of `major-mode' has a language server.
(defun $eglot-ensure ()
  (and (require 'eglot)
       ;; `eglot--lookup-mode' is not autoloaded.  Require `eglot' first.
       buffer-file-name
       (eglot--lookup-mode major-mode)
       (eglot-ensure)))

(setup (:package eglot)

  (:with-mode (prog-mode conf-mode text-mode)
    (:hook $eglot-ensure))

  (add-hook 'before-save-hook '$eglot-before-save-hook)

  (:when-loaded

    (setq eglot-server-programs
          (append '((kotlin-ts-mode "kotlin-language-server")
                    (sql-mode "sqls")
                    ((toml-mode toml-ts-mode)
                     . ("taplo" "lsp" "stdio")))
                  eglot-server-programs))

    )

  )
