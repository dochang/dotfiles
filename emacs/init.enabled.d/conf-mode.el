;;; Conf Mode

;; `conf-mode' isn't a derived mode of `prog-mode'.  Don't run
;; `prog-mode-hook'.
(defun $conf-mode-hook ()
  (whitespace-mode 1))

(req-package conf-mode
  :hook (conf-mode . $conf-mode-hook))
