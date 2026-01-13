(setup lilypond-mode

  (when-let ((init-el (locate-library "lilypond-init")))
    (load init-el 'noerror 'nomessage 'nosuffix 'must-suffix))

  )
