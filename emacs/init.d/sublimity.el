;;; Sublimity
;; [[http://www.emacswiki.org/emacs/Sublimity]]
;; [[https://github.com/zk-phi/sublimity]]

(defun $sublimity-map-setup-hook ()
  (text-scale-set -10))

(req-package sublimity
  :hook (sublimity-map-setup . $sublimity-map-setup-hook)
  :config
  (sublimity-mode -1))
