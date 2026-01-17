(setup hjson-mode

  (unless (package-installed-p 'hjson-mode)
    (package-vc-install '(hjson-mode :url "https://github.com/hjson/hjson-emacs")))

  (setq auto-mode-alist
        (seq-reduce
         (lambda (lst elm)
           (if (member elm lst)
               lst
             (cons elm lst)))
         '(("\\.hjson\\'" . hjson-mode))
         auto-mode-alist))

  )
