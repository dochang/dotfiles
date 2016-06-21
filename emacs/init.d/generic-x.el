;;; Generic Modes

;; Xmodmap Generic Mode
(defun $xmodmap-generic-mode-hook ()
  ($prog-mode-hook*))

(req-package generic-x
  :init
  ;; `generic-extras-enable-list' has to be set *before* loading
  ;; `generic-x'
  ;;
  ;; Disable all generic modes.
  (setq generic-extras-enable-list '())

  ;; `xmodmap-generic-mode' only exists in emacs >= 24
  (when (>= emacs-major-version 24)
    (add-to-list 'generic-extras-enable-list 'xmodmap-generic-mode))

  (add-hook 'xmodmap-generic-mode-hook '$xmodmap-generic-mode-hook))
