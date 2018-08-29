;; zenburn for custom theme
;;
;; [[http://www.emacswiki.org/emacs/ColorThemeZenburn]]
;;
;; 1. [[https://github.com/bbatsov/zenburn-emacs]]
;; 2. [[https://github.com/djcb/elisp/blob/master/themes/zenburn-theme.el]]
;; 3. [[http://www.emacswiki.org/emacs/zenburn.el]]
;;
;; - 1-2 are for custom theme.
;; - 2 is too old.
;; - 3 is an old version of 1.

(req-package zenburn-theme
  :init
  (add-to-list 'safe-local-eval-forms
               '(rainbow-mode 1))
  (add-to-list '**custom-themes** 'zenburn))
