(setup (:package pyim)

  (with-eval-after-load 'pyim

    (pyim-default-scheme 'quanpin)

    (mapc (lambda (key-bindings)
            (apply #'keymap-set pyim-mode-map key-bindings))
          '(("C-@" pyim-select-word-simple)
            ("M-d" pyim-delete-forward-imelem)
            ("C-n" pyim-next-page)
            ("C-p" pyim-previous-page)
            ("C-f" pyim-next-word)
            ("C-b" pyim-previous-word)
            ("<down>" pyim-next-page)
            ("<up>" pyim-previous-page)
            ("<right>" pyim-next-word)
            ("<left>" pyim-previous-word)
            ("M-n" nil)
            ("M-p" nil)))

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
