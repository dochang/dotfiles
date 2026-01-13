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

  )
