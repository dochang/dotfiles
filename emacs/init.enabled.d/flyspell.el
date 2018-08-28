;; Flyspell

(req-package flyspell
  :demand t
  ;; `org' will modify `flyspell-delayed-commands' after `flyspell' loaded.
  ;; Unfortunately we have an init file called `flyspell.el' (this file).  It
  ;; triggers the `eval-after-load' code, but at that moment
  ;; `flyspell-delayed-commands' is undefined.  This will cause an error.
  ;;
  ;; To solve this issue, force to load `flyspell-delayed-commands' before
  ;; loading `org'.
  ;;
  ;; Note: we can't declare `:require flyspell' in `(req-package org)'.
  ;; Because req-package puts `:require flyspell' in the `:config' part of
  ;; `(use-package org)', that means req-package loads flyspell after org.
  :bind (:map flyspell-mode-map
         ("M-t" . nil))
  :hook ((message-setup text-mode) . flyspell-mode)
  :init
  (setq flyspell-use-meta-tab nil))
