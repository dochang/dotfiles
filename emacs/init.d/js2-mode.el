(req-package js2-mode

  :after js

  :init

  ;; NOTE: This variable only takes effect on Emacs < 25.0.  For Emacs >= 25.0,
  ;; see `js-indent-level' in `js-mode'.
  (setq js2-basic-offset js-indent-level)

  ;; Indent "case" in "switch".
  ;;
  ;; NOTE: This variable only takes effect on Emacs < 25.0.  For Emacs >= 25.0,
  ;; see `js-switch-indent-offset' in `js-mode'.
  ;;
  ;; https://github.com/mooz/js2-mode/issues/111#issuecomment-24371116
  (setq js2-indent-switch-body t)

  (setq js2-strict-trailing-comma-warning nil)

  ;; Do not warn about missing semicolon.
  ;;
  ;; Because it does NOT recognize object literals!
  (setq js2-strict-missing-semi-warning nil))
