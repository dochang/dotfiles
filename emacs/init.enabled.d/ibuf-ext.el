(req-package ibuf-ext
  :ensure nil
  :hook (ibuffer-mode . ibuffer-auto-mode)
  ;; `ibuffer-auto-mode' should run in `ibuffer-mode-hook' because if
  ;; should run only once when the buffer created.
  )
