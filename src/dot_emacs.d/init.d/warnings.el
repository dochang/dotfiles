(req-package warnings

  :ensure (warnings :pin :built-in)

  :init

  (setq warning-suppress-types '((lsp-mode)))
  ;; If lsp-mode is unable to calculate the languageId, it will raise many
  ;; warnings and always pop-up the `*Warnings*' buffer.  It's very annoying,
  ;; so suppress warnings from lsp-mode.

  )
