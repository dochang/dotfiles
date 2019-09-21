;; `org-super-agenda' requires Emacs >= 26.1 (required by `ts' library).
;;
;; https://github.com/alphapapa/org-super-agenda/commit/3268cebbe4658ad6d227f4c0e4aa51ecf755dcc4
(when (not (version< emacs-version "26.1"))
  (req-package org-super-agenda
    :hook (emacs-startup . org-super-agenda-mode)))
