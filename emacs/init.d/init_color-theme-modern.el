(req-package color-theme-modern
  :init
  (setq **custom-themes**
        (delete-dups
         (append **custom-themes**
                 ;; color-theme-modern
                 '(tty-dark
                   arjen
                   billw
                   calm-forest
                   clarity
                   classic
                   dark-blue2
                   dark-laptop
                   deep-blue
                   desert
                   euphoria
                   gnome2
                   goldenrod
                   gray30
                   hober
                   jonadabian-slate
                   jonadabian
                   kingsajz
                   late-night
                   lawrence
                   ld-dark
                   midnight
                   oswald
                   pok-wob
                   pok-wog
                   raspopovic
                   renegade
                   resolve
                   retro-orange
                   robin-hood
                   ryerson
                   shaman
                   simple-1
                   sitaramv-solaris
                   subtle-hacker
                   taming-mr-arneson
                   taylor
                   word-perfect
                   subdued)
                 '()))))
