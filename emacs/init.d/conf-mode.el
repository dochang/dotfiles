;;; Conf Mode

;; `conf-mode' isn't derived from `prog-mode'.  Don't call
;; `$prog-mode-hook'.
(defun $conf-mode-hook ()
  (linum-mode 1)
  (whitespace-mode 1))

(req-package conf-mode
  :init
  (add-hook 'conf-mode-hook '$conf-mode-hook))
