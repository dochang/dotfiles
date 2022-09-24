(req-package auto-dim-other-buffers
  :hook ((emacs-startup . (lambda ()
                            (auto-dim-other-buffers-mode
                             (if (or (daemonp) (display-graphic-p)) 1 -1))))))
