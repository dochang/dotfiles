;;; JS Mode

(defun $js-mode-hook ()
  ($prog-mode-hook*)
  ($camel-case-mode 1))

(req-package js
  :loader :built-in
  :init
  (setq js-indent-level 2)
  ;; Indent "case" in "switch".
  ;;
  ;; https://github.com/mooz/js2-mode/issues/111#issuecomment-131096746
  (setq js-switch-indent-offset js-indent-level)
  (setq js-indent-first-init 'dynamic)
  (add-hook 'js-mode-hook '$js-mode-hook))
