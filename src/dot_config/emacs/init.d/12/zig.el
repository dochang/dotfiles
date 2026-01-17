;;; Zig Mode
;; https://github.com/ziglang/zig-mode

(setup (:package zig-mode)

  (:when-loaded

    (setopt zig-format-on-save t)

    )

  )

(setup (:package zig-ts-mode)

  (setq auto-mode-alist
        (seq-reduce
         (lambda (lst elm)
           (if (member elm lst)
               lst
             (cons elm lst)))
         '(("\\.zig\\(?:\\.zon\\)?\\'" . zig-ts-mode))
         auto-mode-alist))

  (:when-loaded

    (with-eval-after-load 'editorconfig

      (setopt editorconfig-indentation-alist
              (cons '(zig-ts-mode zig-ts-mode-indent-offset)
                    editorconfig-indentation-alist))

      )

    (with-eval-after-load 'eglot

      (setq eglot-server-programs
            (cons '(zig-ts-mode "zls")
                  eglot-server-programs))

      )

    (with-eval-after-load 'treesit-auto

      (setq treesit-auto-recipe-list
            (cons (make-treesit-auto-recipe
                   :lang 'zig
                   :ts-mode 'zig-ts-mode
                   :remap 'zig-mode
                   :url "https://github.com/tree-sitter-grammars/tree-sitter-zig"
                   :ext "\\.zig\\(?:\\.zon\\)?\\'")
                  treesit-auto-recipe-list))

      (setopt treesit-auto-langs
              (cons 'zig treesit-auto-langs))

      )

    )

  )
