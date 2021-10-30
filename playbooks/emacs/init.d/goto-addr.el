(req-package goto-addr
  :ensure (goto-addr :pin :built-in)
  :hook ((prog-mode . goto-address-prog-mode)
         (text-mode . goto-address-mode)))
