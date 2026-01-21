(with-eval-after-load 'apheleia

  (setopt apheleia-formatters
          (cons '(prettier-json5 "apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=json5"
                                 (apheleia-formatters-indent "--use-tabs" "--tab-width" 'json5-ts-mode-indent-offset))
                apheleia-formatters))

  )

(setup (:package json5-ts-mode)

  (setq auto-mode-alist
        (seq-reduce
         (lambda (lst elm)
           (if (member elm lst)
               lst
             (cons elm lst)))
         '(("\\.json5\\'" . json5-ts-mode))
         auto-mode-alist))

  (:when-loaded

    (with-eval-after-load 'editorconfig

      (setopt editorconfig-indentation-alist
              (cons '(json5-ts-mode json5-ts-mode-indent-offset)
                    editorconfig-indentation-alist))

      )

    (with-eval-after-load 'treesit-auto

      (setq treesit-auto-recipe-list
            (cons (make-treesit-auto-recipe
                   :lang 'json5
                   :ts-mode 'json5-ts-mode
                   :remap '()
                   :url "https://github.com/Joakker/tree-sitter-json5"
                   :ext "\\.json5\\'")
                  treesit-auto-recipe-list))

      (setopt treesit-auto-langs
              (cons 'json5 treesit-auto-langs))

      )

    (with-eval-after-load 'apheleia

      (setopt apheleia-mode-alist
              (cons '(json5-ts-mode . prettier-json5)
                    apheleia-mode-alist))

      )

    )

  )
