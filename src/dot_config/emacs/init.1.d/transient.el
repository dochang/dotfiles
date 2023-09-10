(setup (:package transient)

  (:require transient)

  (transient-define-prefix $transient-extended ()
    "extended map"
    [
     ["EMMS"
      ("e" "Play in Dired" emms-play-dired)
      ]
     ])

  (:global "C-c x" $transient-extended)

  )
