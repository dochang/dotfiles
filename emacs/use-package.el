(setq use-package-always-defer t)
(setq use-package-always-ensure t)
;; Install packages after `(req-package-finish)'.
;;
;; https://github.com/edvorg/req-package/issues/51#issuecomment-362154387
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package nil 'noerror)
