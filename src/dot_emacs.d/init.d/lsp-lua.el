(defun $reset-lua-language-server-locations ()
  (let ((loc (shell-command-to-string "asdf where lua-language-server"))
        (bin (shell-command-to-string "asdf which lua-language-server")))
    ;; We can't simply use the location of the `shim' or the symlink, since
    ;; lua-language-server expects to find the scripts in a fixed location
    ;; relative to the directory it is run from.
    ;;
    ;; https://github.com/LuaLS/lua-language-server/wiki/Getting-Started#command-line
    (unless (string= loc "")
      (setq lsp-clients-lua-language-server-install-dir
            (concat (string-trim loc) "/"))
      (setq lsp-clients-lua-language-server-bin
            (string-trim bin))
      (setq lsp-clients-lua-language-server-main-location
            (concat lsp-clients-lua-language-server-install-dir "main.lua")))))

(req-package lsp-lua
  :ensure lsp-mode
  :after lsp-mode
  :config
  ($reset-lua-language-server-locations))
