;;; JS Mode

(defun $js-mode-common-hook ()
  ($camel-case-mode 1))

(defun $js-mode-hook ()
  ($js-mode-common-hook))

(req-package js
  :hook (js-mode . $js-mode-hook)
  :custom
  (js-indent-level 2)
  ;; Indent "case" in "switch".
  ;;
  ;; https://github.com/mooz/js2-mode/issues/111#issuecomment-131096746
  (js-switch-indent-offset js-indent-level)
  (js-indent-first-init 'dynamic))
