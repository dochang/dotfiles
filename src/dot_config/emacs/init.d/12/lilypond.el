(with-eval-after-load 'apheleia

  (setopt apheleia-formatters
          (cons '(ly "ly"
                     (format "%s indent-tabs=false; indent; reformat"
                             (if apheleia-formatters-respect-indent-level
                                 (format "indent-width=%d;" LilyPond-indent-level)
                               "")))
                apheleia-formatters))

  )

(setup lilypond-mode

  (when-let* ((init-el (locate-library "lilypond-init")))
    (load init-el 'noerror 'nomessage 'nosuffix 'must-suffix))

  (:when-loaded

    (with-eval-after-load 'editorconfig

      (setopt editorconfig-indentation-alist
              (cons '(LilyPond-mode LilyPond-indent-level)
                    editorconfig-indentation-alist))

      )

    (with-eval-after-load 'apheleia

      (setopt apheleia-mode-alist
              (cons '(LilyPond-mode . ly)
                    apheleia-mode-alist))

      )

    )

  )
