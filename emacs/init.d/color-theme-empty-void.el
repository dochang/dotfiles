(req-package color-theme-empty-void
  :ensure (color-theme-empty-void :pin :el-get-bundle)
  :el-get-bundle (color-theme-empty-void
                  :website "http://www.emacswiki.org/emacs/color-theme-empty-void.el"
                  :description "The Empty Void color theme, by mtvoid (based on sunburst)"
                  :depends ()
                   ;; Do not depend on color-theme, or el-get will install
                   ;; color-theme.  Instead, install color-theme by package.el.
                  :type emacswiki)
  :commands (color-theme-empty-void)
  :init
  (setq **color-themes** ($add-theme **color-themes** 'color-theme-empty-void)))
