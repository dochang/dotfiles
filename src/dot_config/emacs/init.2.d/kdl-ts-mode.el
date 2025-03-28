(setup kdl-ts-mode

  (unless (package-installed-p 'kdl-ts-mode)
    (package-vc-install '(kdl-ts-mode :url "https://github.com/dataphract/kdl-ts-mode")))

  )
