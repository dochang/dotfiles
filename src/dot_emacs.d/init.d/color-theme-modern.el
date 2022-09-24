(req-package color-theme-modern
  :init
  (setq **custom-themes**
        (cl-remove-duplicates
         (append **custom-themes**
                 ;; color-theme-modern
                 '((tty-dark . (lambda () t))
                   (arjen)
                   (billw)
                   (calm-forest)
                   (clarity)
                   (classic)
                   (dark-blue2 . 'display-graphic-p)
                   (dark-laptop)
                   (deep-blue . 'display-graphic-p)
                   (desert)
                   (euphoria)
                   (gnome2)
                   (goldenrod)
                   (gray30)
                   (hober . 'display-graphic-p)
                   ;; Can't see tab on hober
                   (jonadabian-slate)
                   (jonadabian . 'display-graphic-p)
                   (kingsajz . 'display-graphic-p)
                   (late-night)
                   (lawrence)
                   (ld-dark)
                   (midnight)
                   (oswald)
                   (pok-wob)
                   (pok-wog . 'display-graphic-p)
                   (raspopovic)
                   (renegade)
                   (resolve . 'display-graphic-p)
                   ;; (retro-orange)
                   ;;
                   ;; Disable it because of the following error:
                   ;;
                   ;; [Treemacs] Warning: couldn't find hl-line-mode's background
                   ;; color for icons, falling back on unspecified-bg.
                   (robin-hood)
                   (ryerson . 'display-graphic-p)
                   (shaman . 'display-graphic-p)
                   (simple-1)
                   (sitaramv-solaris)
                   (subtle-hacker)
                   (taming-mr-arneson)
                   (taylor)
                   (word-perfect)
                   (subdued))
                 '())
         :key 'car)))
