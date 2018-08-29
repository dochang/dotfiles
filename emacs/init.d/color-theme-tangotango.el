(req-package color-theme-tangotango
  :ensure nil
  :quelpa (color-theme-tangotango
           :fetcher github
           :repo "juba/color-theme-tangotango")
  :commands (color-theme-tangotango)
  :init
  (add-to-list '**color-themes** 'color-theme-tangotango))
