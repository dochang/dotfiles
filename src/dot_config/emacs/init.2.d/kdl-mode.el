(setup kdl-mode

  (unless (package-installed-p 'kdl-mode)
    (package-vc-install "https://github.com/bobuk/kdl-mode"))

  (autoload 'kdl-mode "kdl-mode")

  (setq auto-mode-alist
        (append '(("\\.kdl\\'" . kdl-mode))
                auto-mode-alist))

  )
