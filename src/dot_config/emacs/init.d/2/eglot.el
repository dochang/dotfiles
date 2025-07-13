(defun $eglot-before-save-hook ()
  (require 'eglot)
  ;; `eglot-managed-p' is not autoloaded.  Require `eglot' first.
  (when (eglot-managed-p)
    (ignore-errors (eglot-code-action-organize-imports (point-min) (point-max)))
    ;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md#organizing-imports-with-eglot
    (unless (derived-mode-p '(js-base-mode typescript-ts-base-mode))
      ;; https://www.reddit.com/r/emacs/comments/zam7ja/comment/krww5gs/
      ;; https://github.com/joaotavora/eglot/issues/157
      ;; https://github.com/typescript-language-server/typescript-language-server/issues/202#issuecomment-873469507
      (ignore-errors (eglot-format-buffer)))))

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
