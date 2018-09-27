;;; Generic Modes

;; Xmodmap Generic Mode
(defun $xmodmap-generic-mode-hook ()
  ($run-prog-mode-hook))

(req-package generic-x
  :ensure nil

  :hook ((emacs-startup . (lambda () (require 'generic-x)))
         (xmodmap-generic-mode . $xmodmap-generic-mode-hook))

  :custom
  ;; `generic-extras-enable-list' has to be set *before* loading
  ;; `generic-x'
  ;;
  ;; Disable all generic modes.
  (generic-extras-enable-list '())

  :init

  ;; `xmodmap-generic-mode' only exists in emacs >= 24
  (when (>= emacs-major-version 24)
    (add-to-list 'generic-extras-enable-list 'xmodmap-generic-mode)))
