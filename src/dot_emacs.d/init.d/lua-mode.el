;;; Lua Mode
;; [[http://immerrr.github.com/lua-mode/]]
;; [[https://github.com/immerrr/lua-mode]]

(req-package lua-mode
  :init
  (setq lua-indent-level 2)
  :config
  (require 'lua2-mode nil t))
