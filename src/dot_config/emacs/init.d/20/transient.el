(add-hook 'emacs-startup-hook
          (lambda ()

            (require 'transient)

            (transient-define-prefix $transient-org ()
              "org"
              [
               ["Global"
                ("a" "agenda" org-agenda)
                ("b" "switchb" org-switchb)
                ("c" "capture" org-capture)
                ]
               ["Link"
                ("l" "store link" org-store-link)
                ("L" "insert link global" org-insert-link-global)
                ("o" "open at point global" org-open-at-point-global)
                ]
               ]
              )

            (keymap-global-set "C-c o" #'$transient-org)

            (transient-define-prefix $transient-extended ()
              "extended map"
              [
               ["EMMS"
                ("e" "Play in Dired" emms-play-dired)
                ]
               ["XDG-APPMENU"
                ("@" "Run XDG desktop application" xdg-appmenu)
                ]
               ]
              )

            (keymap-global-set "C-c x" #'$transient-extended)

            ))
