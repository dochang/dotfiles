;; https://www.emacswiki.org/emacs/ColorThemes
;; https://www.emacswiki.org/emacs/ColorThemeCollection
;; http://edward.oconnor.cx/elisp/color-theme-hober2.el
;; http://tess.oconnor.cx/config/
;; https://github.com/genehack/emacs-lisp/blob/master/color-theme-hober2.el

(req-package color-theme-hober2
  :ensure (color-theme-hober2 :pin :el-get-bundle)
  :el-get-bundle (color-theme-hober2
                  :website "https://github.com/genehack/emacs-lisp/blob/master/color-theme-hober2.el"
                  :description "Edward O'Connor's second color theme"
                  :type http
                  :url "https://github.com/genehack/emacs-lisp/raw/master/color-theme-hober2.el")
  :commands (color-theme-hober2)
  :init
  (setq **color-themes** ($add-theme **color-themes** 'color-theme-hober2)))
