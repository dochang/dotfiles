(setup (:package prism)

  ;; https://github.com/alphapapa/prism.el/issues/22
  ;;
  ;; Ensure at least one theme is loaded (by `$set-theme') before enabling
  ;; `prism-mode'.
  (progn

    (:with-mode (prog-mode)
      (:hook (lambda ()
               (when custom-enabled-themes
                 (prism-mode 1)))))

    (:with-mode (python-mode sh-mode)
      (:hook (lambda ()
               (when custom-enabled-themes
                 (prism-mode -1)
                 (prism-whitespace-mode 1)))))

    )

  (:when-loaded

    (:option prism-comments t)

    (:option prism-parens t)

    )

  )
