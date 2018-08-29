;; https://www.emacswiki.org/emacs/ColorThemes
;; https://www.emacswiki.org/emacs/ColorThemeCollection
;; http://edward.oconnor.cx/elisp/color-theme-hober2.el
;; http://tess.oconnor.cx/config/

(req-package color-theme-hober2
  :ensure nil
  :quelpa (color-theme-hober2
           :fetcher url
           :url "http://edward.oconnor.cx/elisp/color-theme-hober2.el")
  :commands (color-theme-hober2)
  :init
  (add-to-list '**color-themes** 'color-theme-hober2))
