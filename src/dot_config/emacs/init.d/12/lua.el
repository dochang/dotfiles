(setup (:package lua-mode)

  (:when-loaded

    (setopt lua-indent-level 2)
    ;; http://lua-users.org/wiki/LuaStyleGuide
    ;; https://github.com/zaki/lua-style-guide
    ;; https://github.com/Olivine-Labs/lua-style-guide/
    ;; http://sputnik.freewisdom.org/en/Coding_Standard
    ;; https://gist.github.com/catwell/b3c01dbea413aa78675740546dfd5ce2
    ;; https://github.com/luarocks/lua-style-guide
    ;; https://love2d.org/forums/viewtopic.php?p=175570#p175570

    )

  )

(setup lua-ts-mode

  (unless (package-built-in-p 'lua-ts-mode)

    (quelpa '(lua-ts-mode :fetcher url
                          :url "https://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/progmodes/lua-ts-mode.el")
            :upgrade nil)
    ;; https://lists.sr.ht/~emacs/emacs-devel/patches/44579
    ;; https://lists.sr.ht/~emacs/emacs-devel/%3C20230911150314.55130-1-jm@pub.pink%3E
    ;;
    ;; Install `lua-ts-mode' by `quelpa' before Emacs 30, because quelpa
    ;; supports the `url' fetcher.  Do not use `package-vc-install', it will
    ;; clone the whole Emacs repository.

    )

  (:when-loaded

    (setopt lua-ts-indent-offset 2)
    ;; http://lua-users.org/wiki/LuaStyleGuide
    ;; https://github.com/zaki/lua-style-guide
    ;; https://github.com/Olivine-Labs/lua-style-guide/
    ;; http://sputnik.freewisdom.org/en/Coding_Standard
    ;; https://gist.github.com/catwell/b3c01dbea413aa78675740546dfd5ce2
    ;; https://github.com/luarocks/lua-style-guide
    ;; https://love2d.org/forums/viewtopic.php?p=175570#p175570

    )

  )
