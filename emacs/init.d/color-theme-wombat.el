(req-package color-theme-wombat
  :ensure nil
  :quelpa (color-theme-wombat
           :fetcher github
           :repo "jasonblewis/color-theme-wombat")
  :commands (color-theme-wombat)
  :init
  (add-to-list '**color-themes** 'color-theme-wombat))
