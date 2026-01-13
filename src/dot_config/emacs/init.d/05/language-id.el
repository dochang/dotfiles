(setup (:package language-id)

  (define-advice language-id-buffer (:around (fn &rest args))
    (or (and (derived-mode-p 'kotlin-ts-mode) "Kotlin")
        (apply fn args)))

  )
