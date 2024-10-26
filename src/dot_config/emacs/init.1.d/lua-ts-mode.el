(setup lua-ts-mode

  (quelpa '(lua-ts-mode :fetcher url
                        :url "https://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/progmodes/lua-ts-mode.el")
          :upgrade nil)
  ;; https://lists.sr.ht/~emacs/emacs-devel/patches/44579
  ;; https://lists.sr.ht/~emacs/emacs-devel/%3C20230911150314.55130-1-jm@pub.pink%3E

  (:when-loaded

    (:option lua-ts-indent-offset 2)
    ;; http://lua-users.org/wiki/LuaStyleGuide
    ;; https://github.com/zaki/lua-style-guide
    ;; https://github.com/Olivine-Labs/lua-style-guide/
    ;; http://sputnik.freewisdom.org/en/Coding_Standard
    ;; https://gist.github.com/catwell/b3c01dbea413aa78675740546dfd5ce2
    ;; https://github.com/luarocks/lua-style-guide
    ;; https://love2d.org/forums/viewtopic.php?p=175570#p175570

    )

  )
