;; Unobtrusively trim lines.  Required by editorconfig.

(req-package ws-butler
  :init
  (setq ws-butler-keep-whitespace-before-point nil)
  ;; Move the point after trimming. Cleanup explicitly.
  )
