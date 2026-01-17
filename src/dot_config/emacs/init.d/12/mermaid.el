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

    )

  )
