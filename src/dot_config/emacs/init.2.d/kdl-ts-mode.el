(setup kdl-ts-mode

  (unless (package-installed-p 'kdl-ts-mode)
    (package-vc-install "https://github.com/dataphract/kdl-ts-mode"))

  )
