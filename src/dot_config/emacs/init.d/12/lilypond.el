(setup lilypond-mode

  (when-let* ((init-el (locate-library "lilypond-init")))
    (load init-el 'noerror 'nomessage 'nosuffix 'must-suffix))

  (:when-loaded

    (with-eval-after-load 'editorconfig

      (setopt editorconfig-indentation-alist
              (cons '(LilyPond-mode LilyPond-indent-level)
                    editorconfig-indentation-alist))

      )

    )

  )
