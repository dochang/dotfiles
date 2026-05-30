(setup hjson-mode

  (unless (package-installed-p 'hjson-mode)
    (package-vc-install '(hjson-mode :url "https://github.com/hjson/hjson-emacs")))

  (setq auto-mode-alist
        (append '(("\\.hjson\\'" . hjson-mode))
                auto-mode-alist))

  )
