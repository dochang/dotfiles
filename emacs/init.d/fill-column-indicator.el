;; Disable Fill-Column-Indicator mode.
;;
;; fci-mode has many issues [1].  DO NOT enable it.
;;
;; [1] https://github.com/alpaker/Fill-Column-Indicator/issues

(req-package fill-column-indicator
  :disabled
  :hook (prog-mode . fci-mode))
