(req-package color-theme-blackboard
  :ensure (color-theme-blackboard :pin :el-get-bundle)
  ;; There are 2 versions of `color-theme-blackboard'.
  ;;
  ;; 1. JD Huntington
  ;;
  ;;     [[http://blog.jdhuntington.com/2008/11/emacs-color-theme-blackboard.html]]
  ;;     [[http://jdhuntington.com/paste/color-theme-blackboard.el.html]]
  ;;
  ;; 2. Jason Lewis <jason@dickson.st>
  ;;
  ;;     [[https://github.com/jasonblewis/color-theme-blackboard]]
  ;;
  ;; The 2nd version just add a copyright information and a `provide' for
  ;; the 1st one.  We use it.
  ;;
  ;; We can't install it via quelpa due to this error:
  ;;
  ;; ```
  ;; Package lacks a "Version" or "Package-Version" header
  ;; ```
  ;;
  ;; Use el-get instead.
  :el-get-bundle (color-theme-blackboard
                  :website "https://github.com/jasonblewis/color-theme-blackboard"
                  :description "Blackboard Colour Theme for Emacs."
                  :depends ()
                   ;; Do not depend on color-theme, or el-get will install
                   ;; color-theme.  Instead, install color-theme by package.el.
                  :type github
                  :pkgname "jasonblewis/color-theme-blackboard")
  :commands (color-theme-blackboard)
  :init
  (setq **color-themes** ($add-theme **color-themes** 'color-theme-blackboard)))
