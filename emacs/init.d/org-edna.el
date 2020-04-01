;; Emacs runs `org-load-hook' right after loading `org', even before
;; `eval-after-load'.  Also, the `:init' section of `org-edna' may be ran after
;; `org' loaded.  So we have to add the hook function to `org-load-hook' once
;; this file loaded.
;;
;; When `:demand t', the execution order of `use-package' is:
;;
;; :init
;; (require 'org)
;; :hook
(add-hook 'org-load-hook 'org-edna-mode)

(req-package org-edna)
