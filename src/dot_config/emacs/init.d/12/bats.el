(setup (:package bats-mode)

  (:when-loaded

    (with-eval-after-load 'apheleia

      (setopt apheleia-mode-alist
              (cons '(bats-mode . shfmt)
                    apheleia-mode-alist))

      )

    )

  )
