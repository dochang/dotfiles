(setup (:package kotlin-mode))

(setup (:package kotlin-ts-mode)

  (:when-loaded

    (with-eval-after-load 'eglot

      (setq eglot-server-programs
            (cons '(kotlin-ts-mode "kotlin-language-server")
                  eglot-server-programs))

      )

    )

  )
