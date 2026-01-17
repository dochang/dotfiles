(setup (:package toml-mode)

  (:file-match "Pipfile\\'")

  (:when-loaded

    (with-eval-after-load 'eglot

      (setq eglot-server-programs
            (cons '(toml-mode . ("taplo" "lsp" "stdio"))
                  eglot-server-programs))

      )

    )

  )

(setup (:package toml-ts-mode)

  (:when-loaded

    (with-eval-after-load 'eglot

      (setq eglot-server-programs
            (cons '(toml-ts-mode . ("taplo" "lsp" "stdio"))
                  eglot-server-programs))

      )

    )

  )
