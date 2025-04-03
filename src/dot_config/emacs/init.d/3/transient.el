(setup (:package transient)

  (add-hook 'emacs-startup-hook
            (lambda ()

              (require 'transient)

              (transient-define-prefix $transient-extended ()
                "extended map"
                [
                 ["EMMS"
                  ("e" "Play in Dired" emms-play-dired)
                  ]
                 ])

              (keymap-global-set "C-c x" '$transient-extended)

              ))

  )
