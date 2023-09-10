(setup ibuf-ext

  (:with-mode (ibuffer-mode)
    (:hook ibuffer-auto-mode))
  ;; `ibuffer-auto-mode' should run in `ibuffer-mode-hook' because it should
  ;; run only once when the buffer created.

  )
