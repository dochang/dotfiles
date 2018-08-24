;;; Sass Mode
;; [[https://github.com/nex3/sass-mode]]

(defun $sass-mode-hook ()
  ;; No need to run `prog-mode-hook', `$haml-mode-hook' will do this since
  ;; `sass-mode' is derived from `haml-mode'.
  (unless (derived-mode-p 'haml-mode)
    ($run-prog-mode-hook))
  (rainbow-mode 1))

(req-package sass-mode
  :init
  (add-hook 'sass-mode-hook '$sass-mode-hook))
