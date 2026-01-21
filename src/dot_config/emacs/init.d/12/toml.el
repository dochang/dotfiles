(with-eval-after-load 'apheleia

  (setopt apheleia-formatters
          (cons '(taplofmt "taplo" "fmt" "-")
                apheleia-formatters))

  )

(setup (:package toml-mode)

  (:file-match "Pipfile\\'")

  (:when-loaded

    (with-eval-after-load 'eglot

      (setq eglot-server-programs
            (cons '(toml-mode . ("taplo" "lsp" "stdio"))
                  eglot-server-programs))

      )

    (with-eval-after-load 'apheleia

      (setopt apheleia-mode-alist
              (cons '(toml-mode . taplofmt)
                    apheleia-mode-alist))

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

    (with-eval-after-load 'apheleia

      (setopt apheleia-mode-alist
              (cons '(toml-ts-mode . taplofmt)
                    apheleia-mode-alist))

      )

    )

  )
