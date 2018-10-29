;; Lua Block Mode
;;
;; [[http://www.emacswiki.org/emacs/LuaBlockMode]]

(req-package lua-block
  :ensure (lua-block :pin :quelpa)
  :quelpa (lua-block :fetcher github :repo "emacsmirror/lua-block")
  :hook (lua-mode . lua-block-mode)
  :custom
  (lua-block-highlight-toggle t))
