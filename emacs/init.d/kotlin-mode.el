(defun $kotlin-mode-hook ()
  ($camel-case-mode 1))

(req-package kotlin-mode
  ;; For Kotlin script
  ;;
  ;; [[https://kotlinlang.org/docs/tutorials/command-line.html#using-the-command-line-to-run-scripts]]
  :mode ("\\.kts\\'" . kotlin-mode)
  :hook (kotlin-mode . $kotlin-mode-hook)
  :custom
  (kotlin-tab-width 4))
