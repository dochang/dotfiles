(defun $web-mode-hook ()
  (when (and web-mode-enable-engine-detection
             (local-variable-p 'engine))
    (web-mode-set-engine (buffer-local-value 'engine))))

(setup (:package web-mode)

  (:hook $web-mode-hook)

  (:when-loaded

    (:option web-mode-enable-engine-detection t)

    )

  )
