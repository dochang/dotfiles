;;; indent-guide
;; [[https://github.com/zk-phi/indent-guide]]

(req-package indent-guide
  :hook (prog-mode . indent-guide-mode)
  :custom
  ;; DO NOT draw all guide lines.  It's very slow when there're too many
  ;; levels.
  (indent-guide-recursive nil))
