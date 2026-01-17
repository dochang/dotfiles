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

    )

  )
