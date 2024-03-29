;;; JS Mode

(defun $js-mode-common-hook ()
  ($camel-case-mode 1))

(defun $js-mode-hook ()
  ($js-mode-common-hook))

(req-package js
  :hook (js-mode . $js-mode-hook)
  :init
  (setq js-indent-level 2)
  ;; Indent "case" in "switch".
  ;;
  ;; https://github.com/mooz/js2-mode/issues/111#issuecomment-131096746
  (setq js-switch-indent-offset js-indent-level)
  (setq js-indent-first-init 'dynamic))
