(setup (:package pyim)

  (with-eval-after-load 'pyim

    (pyim-default-scheme 'quanpin)

    )

  (with-eval-after-load 'pyim-cloudim

    (setopt pyim-cloudim nil)

    )

  (with-eval-after-load 'pyim-page

    (setopt pyim-page-length 10)

    )

  (with-eval-after-load 'pyim-pinyin

    (setopt pyim-pinyin-fuzzy-alist '())

    )

  )
