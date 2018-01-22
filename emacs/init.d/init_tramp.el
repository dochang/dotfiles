;;; Tramp

(defun $add-tramp-environments ()
  ;; Required by `tramp-remote-process-environment'.
  (require 'tramp-sh)
  (mapc (lambda (var)
          (add-to-list 'tramp-remote-process-environment var))
        '("GIT_PAGER=cat" "PAGER=cat" "LANGUAGE=C" "LANG=C" "LC_ALL=")))

(req-package tramp
  :config
  ($add-tramp-environments))
