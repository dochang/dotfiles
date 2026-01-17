(with-eval-after-load 'apheleia

  (setopt apheleia-formatters
          (cons '(google-java-format "google-java-format" "--aosp" "-")
                apheleia-formatters))

  )
