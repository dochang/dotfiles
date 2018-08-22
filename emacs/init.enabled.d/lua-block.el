;; Lua Block Mode
;;
;; [[http://www.emacswiki.org/emacs/LuaBlockMode]]

(req-package lua-block
  :ensure nil
  :quelpa (lua-block :fetcher github :repo "emacsmirror/lua-block")
  :init
  (setq lua-block-highlight-toggle t))
