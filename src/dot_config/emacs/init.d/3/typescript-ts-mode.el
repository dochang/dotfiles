(setup (:package typescript-ts-mode)

  (setq auto-mode-alist
        (seq-reduce
         (lambda (lst elm)
           (if (member elm lst)
               lst
             (cons elm lst)))
         '(("\\.ts\\'" . typescript-ts-mode)
           ("\\.tsx\\'" . tsx-ts-mode))
         auto-mode-alist))

  )
