;; https://www.emacswiki.org/emacs/WhiteSpace
;; http://xahlee.info/emacs/emacs/whitespace-mode.html

(setup whitespace

  (:with-mode (prog-mode conf-mode)
    (:hook whitespace-mode))

  (:when-loaded

    (:option whitespace-style '(face trailing tabs lines-tail newline))
    ;; `tab-mark` hurts my eyes.  Do not include it.

    )

  )
