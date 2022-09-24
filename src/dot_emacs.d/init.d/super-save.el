;; Some functions like `lv-window' always triggers `super-save-command', it's
;; very annoying.  Put them in the blacklist.  See [1] & [2] for details.
;;
;; [1]: https://github.com/jming422/emacs-config/commit/5f61ad97bad46235c7fb4d5650962f2c32e1fde1
;; [2]: https://github.com/emacs-lsp/lsp-mode/issues/1322
(define-advice super-save-command (:around (fn &rest r) skip-blacklist)
  (let ((blacklist '(lv-window))
        (skip nil))
    (mapbacktrace (lambda (evald fun args flags)
                    (when (member fun blacklist)
                      (setq skip t)))
                  'super-save-command)
    (unless skip
      (apply fn r))))

(req-package super-save
  :hook (emacs-startup . super-save-mode)
  :init
  (setq super-save-auto-save-when-idle t))
