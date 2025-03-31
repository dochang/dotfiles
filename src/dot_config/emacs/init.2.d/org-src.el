(setup org-src
  (:package org)

  (:when-loaded

    ;; Preserve leading whitespace characters on export and when
    ;; switching between the org buffer and the language mode edit
    ;; buffer.  This variable is especially useful for tangling
    ;; languages such as Python or Makefile, in which whitespace
    ;; indentation in the output is critical.
    (:option org-src-preserve-indentation t)
    ;; Don't indent for the content of a source code block.  NOTE: It
    ;; has no effect if `org-src-preserve-indentation' is non-nil.  But
    ;; we still set it to 0 here.
    (:option org-edit-src-content-indentation 0)

    )

  )
