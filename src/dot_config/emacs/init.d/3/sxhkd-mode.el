(setup sxhkd-mode

  (unless (package-installed-p 'sxhkd-mode)
    (package-vc-install '(sxhkd-mode :url "https://github.com/xFA25E/sxhkd-mode")))

  (setq auto-mode-alist
        (append '(("/sxhkdrc\\'" . sxhkd-mode))
                auto-mode-alist))

  (:when-loaded

    (setopt sxhkd-mode-reload-config nil)

    )

  )
