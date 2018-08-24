;;; Scala Mode

(defun $scala-mode-hook ()
  ($run-prog-mode-hook)
  ($camel-case-mode 1)
  ;; Flycheck is too strict for `*.sbt'.  Use flymake instead.
  (when (or (null buffer-file-name)
            ($file-name-match "\\.sbt\\'" buffer-file-name))
    (flycheck-mode -1)
    (flymake-mode 1)))

(req-package scala-mode
  :hook (scala-mode . $scala-mode-hook))
