(setup (:package json-mode)

  (setq auto-mode-alist
        (append auto-mode-alist
                '(("\\.json[c5]\\'" . jsonc-mode))))
  ;; Append into the end because they're default values.

  )
