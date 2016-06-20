;;; Lua Mode
;; [[http://immerrr.github.com/lua-mode/]]
;; [[https://github.com/immerrr/lua-mode]]

(defun $lua-mode-hook ()
  ($prog-mode-hook*)
  (lua-block-mode 1))

(req-package lua-mode
  :init
  (setq lua-indent-level 2)
  (add-hook 'lua-mode-hook '$lua-mode-hook)
  :config
  (require 'lua2-mode nil t))
