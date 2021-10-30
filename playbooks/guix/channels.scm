(use-modules (guix ci))

(cons (channel-with-substitutes-available
       (channel
        (inherit %default-guix-channel)
        (url "{{ dotfiles_guix_default_channel_url }}"))
       "https://ci.guix.gnu.org"
       ;; The API required by `channel-with-substitutes-available' only exists
       ;; on Guix CI Server.
       )
      (delq %default-guix-channel %default-channels))
