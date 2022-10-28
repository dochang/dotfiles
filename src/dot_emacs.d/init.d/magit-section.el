;; `magit-section-maybe-paint-visibility-ellipses' will be called when Emacs is
;; ran as daemon mode.  I don't know the reason so far.  Since
;; `magit-current-section' returns nil, `oref' will raise an error.  The
;; workaround is check the return value of `magit-current-section' before
;; `magit-section-maybe-paint-visibility-ellipses' is called.
;;
;; The following patch fixes another issue, but the root cause is the same.
;;
;; https://github.com/magit/magit/issues/4481
;; https://github.com/magit/magit/commit/07e172a11fccf4ed4e4fb4f0109e467ac5c0fe5b
(define-advice magit-section-maybe-paint-visibility-ellipses (:before-while (&rest args))
  (or (magit-region-sections)
      (magit-current-section)))

(req-package magit-section)
