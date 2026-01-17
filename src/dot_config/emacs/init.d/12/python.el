(setup (:package python)

  (:when-loaded

    (with-eval-after-load 'apheleia

      (setopt apheleia-mode-alist
              (append '((python-ts-mode . (black isort))
                        (python-mode . (black isort)))
                      apheleia-mode-alist))

      )

    )

  )
