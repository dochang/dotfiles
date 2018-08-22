(req-package color-theme-blackboard
  :ensure nil
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
  :el-get t
  :commands (color-theme-blackboard)
  :init
  (add-to-list '**color-themes** 'color-theme-blackboard))
