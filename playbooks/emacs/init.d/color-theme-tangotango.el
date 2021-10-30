(req-package color-theme-tangotango
  :ensure (color-theme-tangotango :pin :quelpa)
  :quelpa (color-theme-tangotango
           :fetcher github
           :repo "juba/color-theme-tangotango")
  :commands (color-theme-tangotango)
  :init
  (setq **color-themes** ($add-theme **color-themes** 'color-theme-tangotango)))
