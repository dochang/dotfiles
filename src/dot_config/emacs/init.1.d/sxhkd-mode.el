(setup sxhkd-mode

  (unless (package-installed-p 'sxhkd-mode)
    (package-vc-install "https://github.com/xFA25E/sxhkd-mode"))

  (setq auto-mode-alist
        (append '(("/sxhkdrc\\'" . sxhkd-mode))
                auto-mode-alist))

  (:when-loaded

    (:option sxhkd-mode-reload-config nil)

    )

  )
