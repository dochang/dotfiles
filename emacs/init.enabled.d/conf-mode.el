;;; Conf Mode

;; `conf-mode' isn't a derived mode of `prog-mode'.  Don't run
;; `prog-mode-hook'.
(defun $conf-mode-hook ()
  (linum-mode 1)
  (whitespace-mode 1))

(req-package conf-mode
  :init
  (add-hook 'conf-mode-hook '$conf-mode-hook))
