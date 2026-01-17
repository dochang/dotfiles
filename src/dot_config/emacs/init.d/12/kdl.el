(setup kdl-mode

  (unless (package-installed-p 'kdl-mode)
    (package-vc-install '(kdl-mode :url "https://github.com/bobuk/kdl-mode")))

  (autoload 'kdl-mode "kdl-mode")

  (setq auto-mode-alist
        (append '(("\\.kdl\\'" . kdl-mode))
                auto-mode-alist))

  (:when-loaded

    (with-eval-after-load 'editorconfig

      (setopt editorconfig-indentation-alist
              (cons '(kdl-mode tab-width)
                    editorconfig-indentation-alist))

      )

    )

  )

(setup kdl-ts-mode

  (unless (package-installed-p 'kdl-ts-mode)
    (package-vc-install '(kdl-ts-mode :url "https://github.com/dataphract/kdl-ts-mode")))

  (:when-loaded

    (with-eval-after-load 'editorconfig

      (setopt editorconfig-indentation-alist
              (cons '(kdl-ts-mode kdl-ts-mode-indent-offset)
                    editorconfig-indentation-alist))

      )

    (with-eval-after-load 'treesit-auto

      (setq treesit-auto-recipe-list
            (cons (make-treesit-auto-recipe
                   :lang 'kdl
                   :ts-mode 'kdl-ts-mode
                   :remap 'kdl-mode
                   :url "https://github.com/tree-sitter-grammars/tree-sitter-kdl"
                   :ext "\\.kdl\\'")
                  treesit-auto-recipe-list))

      (setopt treesit-auto-langs
              (cons 'kdl treesit-auto-langs))

      )

    )

  )
