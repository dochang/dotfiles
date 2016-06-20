;;; FVWM Mode
;; [[http://www.emacswiki.org/FvwmMode]]
;; [[http://www.lair.be/projects_fvwm-mode.php]]

(defun $fvwm-mode-hook ()
  ($prog-mode-hook*))

(req-package fvwm-mode
  :mode (("\\.fvwm2rc\\'" . fvwm-mode)
         ("/\\.fvwm/config\\'" . fvwm-mode))
  :init
  (add-hook 'fvwm-mode-hook '$fvwm-mode-hook))
