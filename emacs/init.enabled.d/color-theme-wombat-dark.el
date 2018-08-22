;; This theme is a fork of `color-theme-wombat+`.  We won't use the
;; original fork since the site is too slow to download the file.
;;
;; [[http://jaderholm.com/color-themes/color-theme-wombat+.el]]
;; [[http://jaderholm.com/color-themes/color-theme-wombat+.el.sept2013]]

(req-package color-theme-wombat-dark
  :ensure nil
  :quelpa (color-theme-wombat-dark
           :fetcher github
           :repo "leoncamel/color-theme-wombat-dark")
  :commands (color-theme-wombat-dark)
  :init
  (add-to-list '**color-themes** 'color-theme-wombat-dark))
