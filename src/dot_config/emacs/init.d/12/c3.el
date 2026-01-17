(setup c3-ts-mode

  (unless (package-installed-p 'c3-ts-mode)
    (package-vc-install '(c3-ts-mode :url "https://github.com/c3lang/c3-ts-mode")))

  (setq auto-mode-alist
        (seq-reduce
         (lambda (lst elm)
           (if (member elm lst)
               lst
             (cons elm lst)))
         '(("\\.c3[it]?\\'" . c3-ts-mode))
         auto-mode-alist))

  (:when-loaded

    (with-eval-after-load 'editorconfig

      (setopt editorconfig-indentation-alist
              (cons '(c3-ts-mode c3-ts-mode-indent-offset)
                     editorconfig-indentation-alist))

      )

    (with-eval-after-load 'eglot

      (setq eglot-server-programs
            (cons '(c3-ts-mode "c3lsp")
                   eglot-server-programs))

      )

    )

  )
