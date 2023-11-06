(setup lua-ts-mode

  (unless (package-installed-p 'lua-ts-mode)
    (package-vc-install "https://git.sr.ht/~johnmuhl/lua-ts-mode"))

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
