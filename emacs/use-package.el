(defun $use-package-ensure-filter (name arg state)
  (pcase arg
    (`(,_ . ,(pred keywordp)) nil)
    (_ arg)))

(defun $use-package-ensure-function (name args state &optional no-refresh)
  (use-package-ensure-elpa name
                           (mapcar
                            (lambda (arg)
                              ($use-package-ensure-filter name arg state))
                            args)
                           state
                           no-refresh))

(setq use-package-ensure-function '$use-package-ensure-function)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)
;; Install packages after `(req-package-finish)'.
;;
;; https://github.com/edvorg/req-package/issues/51#issuecomment-362154387
(unless (package-installed-p 'use-package)
  (unless (assq 'use-package package-archive-contents)
    (package-refresh-contents))
  (package-install 'use-package))
(require 'use-package nil 'noerror)
