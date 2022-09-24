(req-package lsp-go
  :ensure lsp-mode
  :after lsp-mode
  :init
  (setq lsp-clients-go-format-tool
        (cond ((executable-find "goimports") "goimports")
              (t "gofmt")))
  :config
  (lsp-register-custom-settings
   '(
     ("gopls.completeUnimported" t t)
     ;; https://github.com/microsoft/vscode-go/issues/2441#issuecomment-544603601
     ))
  )
