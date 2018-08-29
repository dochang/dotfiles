;; Midnight Mode
;; [[info:emacs#Kill%20Buffer]]
(req-package midnight
  :hook (emacs-startup . (lambda () (require 'midnight))))
