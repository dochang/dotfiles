(setup (:package zig-ts-mode)

  (setq auto-mode-alist
        (seq-reduce
         (lambda (lst elm)
           (if (member elm lst)
               lst
             (cons elm lst)))
         '(("\\.zig\\(?:\\.zon\\)?\\'" . zig-ts-mode))
         auto-mode-alist))

  )
