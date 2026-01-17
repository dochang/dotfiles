(with-eval-after-load 'apheleia

  (setopt apheleia-formatters
          (cons '(hurlfmt "hurlfmt" "--no-color" "--in" "hurl" "--out" "hurl")
                apheleia-formatters))

  )

(setup hurl-mode

  (unless (package-installed-p 'hurl-mode)
    (package-vc-install '(hurl-mode :url "https://github.com/JasZhe/hurl-mode")))

  (setq auto-mode-alist
        (append '(("\\.hurl\\'" . hurl-mode))
                auto-mode-alist))

  (:when-loaded

    (setopt hurl-mode-use-json-ts-mode t)

    (setopt hurl-use-fast-process-settings t)

    (with-eval-after-load 'apheleia

      (setopt apheleia-mode-alist
              (cons '(hurl-mode . hurlfmt)
                    apheleia-mode-alist))

      )

    )

  )
