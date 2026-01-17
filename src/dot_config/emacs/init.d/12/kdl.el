(setup kdl-mode

  (unless (package-installed-p 'kdl-mode)
    (package-vc-install '(kdl-mode :url "https://github.com/bobuk/kdl-mode")))

  (autoload 'kdl-mode "kdl-mode")

  (setq auto-mode-alist
        (append '(("\\.kdl\\'" . kdl-mode))
                auto-mode-alist))

  )

(setup kdl-ts-mode

  (unless (package-installed-p 'kdl-ts-mode)
    (package-vc-install '(kdl-ts-mode :url "https://github.com/dataphract/kdl-ts-mode")))

  )
