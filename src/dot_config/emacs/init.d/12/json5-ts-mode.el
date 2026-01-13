(setup (:package json5-ts-mode)

  (setq auto-mode-alist
        (seq-reduce
         (lambda (lst elm)
           (if (member elm lst)
               lst
             (cons elm lst)))
         '(("\\.json5\\'" . json5-ts-mode))
         auto-mode-alist))

  )
