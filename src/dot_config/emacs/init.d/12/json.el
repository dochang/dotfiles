(setup (:package json-mode)

  (setq auto-mode-alist
        (append auto-mode-alist
                '(("\\.jsonc\\'" . jsonc-mode))))
  ;; Append into the end because they're default values.

  )

(setup (:package json-ts-mode)

  (:when-loaded

    (with-eval-after-load 'treesit-auto

      (when-let* ((recipe
                   (seq-find (lambda (recipe)
                               (eq 'json (treesit-auto-recipe-lang recipe)))
                             treesit-auto-recipe-list)))
        (setf (treesit-auto-recipe-remap recipe)
              (seq-uniq
               (append '(json-mode jsonc-mode)
                       (ensure-list (treesit-auto-recipe-remap recipe))))))
      ;; Remap `json-mode' and `jsonc-mode' to `json-ts-mode'

      )

    )

  )
