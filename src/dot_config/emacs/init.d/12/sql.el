(setup (:package sql)

  (:when-loaded

    (with-eval-after-load 'eglot

      (setq eglot-server-programs
            (cons '(sql-mode "sqls")
                  eglot-server-programs))

      )

    )

  )
