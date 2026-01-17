(with-eval-after-load 'apheleia

  (setopt apheleia-formatters
          (cons '(sql-formatter "sql-formatter")
                apheleia-formatters))

  )

(setup (:package sql)

  (:when-loaded

    (with-eval-after-load 'eglot

      (setq eglot-server-programs
            (cons '(sql-mode "sqls")
                  eglot-server-programs))

      )

    (with-eval-after-load 'apheleia

      (setopt apheleia-mode-alist
              (cons '(sql-mode . sql-formatter)
                    apheleia-mode-alist))

      )

    )

  )
