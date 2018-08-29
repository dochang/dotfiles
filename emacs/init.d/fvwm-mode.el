;;; FVWM Mode
;; [[http://www.emacswiki.org/FvwmMode]]
;; [[http://www.lair.be/projects_fvwm-mode.php]]

(req-package fvwm-mode
  :mode (("\\.fvwm2rc\\'" . fvwm-mode)
         ("/\\.fvwm/config\\'" . fvwm-mode)))
