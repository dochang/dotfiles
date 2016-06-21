;;; indent-guide
;; [[https://github.com/zk-phi/indent-guide]]

(req-package indent-guide
  :init
  ;; DO NOT draw all guide lines.  It's very slow when there're too many
  ;; levels.
  (setq indent-guide-recursive nil))
