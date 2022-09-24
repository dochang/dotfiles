(req-package server
  :hook (emacs-startup . (lambda ()
                           (unless (or (bound-and-true-p server-process)
                                       (and (fboundp 'server-running-p)
                                            (server-running-p server-name)))
                             (server-start))))
  :init
  (setq server-name (if (stringp (daemonp))
                        (daemonp)
                      (number-to-string (emacs-pid))))
  ;; If Emacs is invoked with `--daemon=NAME', the name of server communication
  ;; file will be NAME, which is returned by `daemonp'.  Otherwise, if Emacs is
  ;; invoked with `--daemon' or no `--daemon', set `server-name' to Emacs pid.
  ;;
  ;; Actually Emacs changes `server-name' before `server-start' if
  ;; `--daemon=NAME' is passed.  We do this explicitly for convenience of
  ;; reloading configuration.
  ;;
  ;; When `--daemon[=NAME]' is passed, Emacs starts the server after
  ;; `after-init-hook', before `emacs-startup-hook'.
  ;;
  ;; Order:
  ;;
  ;;   - Setting `server-name' in `load-dirs', which is in `after-init-hook'.
  ;;   - `server-start'
  ;;   - `emacs-startup-hook'
  (setq server-use-tcp t))
