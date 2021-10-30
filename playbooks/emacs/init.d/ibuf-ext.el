(req-package ibuf-ext
  :ensure (ibuf-ext :pin :built-in)
  :hook (ibuffer-mode . ibuffer-auto-mode)
  ;; `ibuffer-auto-mode' should run in `ibuffer-mode-hook' because if
  ;; should run only once when the buffer created.
  )
