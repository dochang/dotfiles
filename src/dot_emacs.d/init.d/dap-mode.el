(req-package dap-mode
  :hook ((emacs-startup . dap-mode)
         (emacs-startup . dap-ui-mode)
         (emacs-startup . dap-tooltip-mode)))
