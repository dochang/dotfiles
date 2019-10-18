(req-package color-theme-wombat
  :ensure (color-theme-wombat :pin :quelpa)
  :quelpa (color-theme-wombat
           :fetcher github
           :repo "jasonblewis/color-theme-wombat")
  :commands (color-theme-wombat)
  :init
  (setq **color-themes** ($add-theme **color-themes** 'color-theme-wombat)))
