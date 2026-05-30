(setup (:package mermaid-mode)

  (:when-loaded

    (setopt mermaid-indentation-level 4)

    (with-eval-after-load 'editorconfig

      (setopt editorconfig-indentation-alist
              (cons '(mermaid-mode mermaid-indentation-level)
                    editorconfig-indentation-alist))

      )

    )

  )

(setup (:package mermaid-ts-mode)

  (:when-loaded

    (setopt mermaid-ts-indent-level 4)

    (with-eval-after-load 'editorconfig

      (setopt editorconfig-indentation-alist
              (cons '(mermaid-ts-mode mermaid-ts-indent-level)
                    editorconfig-indentation-alist))

      )

    (with-eval-after-load 'treesit-auto

      (setq treesit-auto-recipe-list
            (cons (make-treesit-auto-recipe
                   :lang 'mermaid
                   :ts-mode 'mermaid-ts-mode
                   :remap 'mermaid-mode
                   :url "https://github.com/monaqa/tree-sitter-mermaid"
                   :ext "\\.mmd\\'")
                  treesit-auto-recipe-list))

      (setopt treesit-auto-langs
              (cons 'mermaid treesit-auto-langs))

      )

    )

  )
