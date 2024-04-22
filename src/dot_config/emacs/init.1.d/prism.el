(setup (:package prism)

  ;; https://github.com/alphapapa/prism.el/issues/22
  ;;
  ;; Ensure at least one theme is loaded (by `$set-theme') before enabling
  ;; `prism-mode'.
  ;;
  ;; Also ensure Emacs is running in a graphic display.
  (progn

    (:with-mode (prog-mode)
      (:hook (lambda ()
               (when (and (display-graphic-p)
                          custom-enabled-themes)
                 (prism-mode 1)))))

    (:with-mode (python-base-mode sh-base-mode)
      (:hook (lambda ()
               (when (and (display-graphic-p)
                          custom-enabled-themes)
                 (prism-mode -1)
                 (prism-whitespace-mode 1)))))

    )

  (:when-loaded

    (:option prism-comments t)

    (:option prism-parens t)

    )

  )
