;;; Scss Mode
;; [[https://github.com/antonj/scss-mode]]

(defun $scss-mode-hook ()
  ;; No need to run `prog-mode-hook' if `scss-mode' is derived from `css-mode'.
  (unless (derived-mode-p 'css-mode)
    ($run-prog-mode-hook))
  (rainbow-mode 1))

(req-package scss-mode
  :init
  ;; Don't compile after saving.
  (setq scss-compile-at-save nil)
  (add-hook 'scss-mode-hook '$scss-mode-hook))
