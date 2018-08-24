;; We need this wrapper because `copyright-update-year' will cause Emacs goes
;; into an infinite loop if super-save enabled.
;;
;; The call stack:
;;
;; `copyright-update'
;; -> `copyright-update-year'
;; -> `switch-to-buffer'
;; -> `super-save-command-advice'
;; -> `super-save-command'
;; -> `save-buffer'
;; -> `before-save-hook'
;; -> `copyright-update'
;;
(define-advice copyright-update (:around (fn &rest r) super-save)
  (unwind-protect
      (progn
        (when (featurep 'super-save)
          (super-save-remove-advice-from-trigger-commands))
        (apply fn r))
    (when (featurep 'super-save)
      (super-save-advise-trigger-commands))))

(req-package copyright
  :hook (before-save . copyright-update))
