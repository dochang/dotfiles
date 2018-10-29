;; zenburn for color theme
;;
;; [[http://www.emacswiki.org/emacs/ColorThemeZenburn]]
;;
;; 1. [[https://github.com/dbrock/zenburn-el]]
;; 2. [[https://github.com/bbatsov/zenburn-emacs/blob/0c46ca823dd007241c48778d38b80ac8bde6d5ee/color-theme-zenburn.el]]
;;
;; 1 supports more packages than 2.

(req-package zenburn
  :ensure (zenburn :pin :quelpa)
  :quelpa (zenburn :fetcher github :repo "dbrock/zenburn-el")
  :init
  (add-to-list '**color-themes** 'color-theme-zenburn))
