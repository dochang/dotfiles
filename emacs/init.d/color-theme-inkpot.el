(req-package color-theme-inkpot
  :ensure (color-theme-inkpot :pin :el-get-bundle)
  ;; There are 2 versions on EmacsWiki.
  ;;
  ;; 1. [[http://www.emacswiki.org/emacs/ColorThemeInkpot]]
  ;; 2. [[http://www.emacswiki.org/emacs/color-theme-inkpot.el]]
  ;;
  ;; They're the same.  Version 2 just add description and copyright.
  :el-get-bundle (color-theme-inkpot
                  :website "http://www.emacswiki.org/emacs/ColorThemeInkpot"
                  :description "Color theme based on the Inkpot theme. Ported and tweaked by Per Vognsen."
                  :depends ()
                   ;; There are 2 versions on EmacsWiki.
                   ;;
                   ;; 1. [[http://www.emacswiki.org/emacs/ColorThemeInkpot]]
                   ;; 2. [[http://www.emacswiki.org/emacs/color-theme-inkpot.el]]
                   ;;
                   ;; They're the same.  Version 2 just add description and copyright.
                  :type emacswiki)
  :commands (color-theme-inkpot)
  :init
  (setq **color-themes** ($add-theme **color-themes** 'color-theme-inkpot)))
