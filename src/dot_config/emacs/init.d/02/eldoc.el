(setup (:package eldoc)

  (:when-loaded

    (setopt eldoc-echo-area-use-multiline-p nil)
    ;; Always truncate the doc string.
    ;;
    ;; DO NOT resize the echo area.  It's annoying.

    )

  )
