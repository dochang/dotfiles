(setq gnus-select-method '(nnnil "")
      gnus-secondary-select-methods
      (append
       ;; '((nnimap "dochang@gmail.com"
       ;;           (nnimap-address "imap.gmail.com")
       ;;           (nnimap-server-port 993)
       ;;           (nnimap-stream ssl)))
       (mapcar (lambda (dir)
                 `(nnmaildir ,(file-name-nondirectory dir) (directory ,dir)))
               (directory-files (or (getenv "MAILDIR") "~/Maildir/") t "^[^.]")))
      ;; Prompt user *just once* for a file name for each invocation
      ;; of the saving commands.
      ;;
      ;; [[info:gnus#Saving%20Articles]]
      gnus-prompt-before-saving t
      ;; Sort articles in the following order:
      ;;
      ;; 1. date
      ;; 2. number
      ;;
      ;; [[info:gnus#Sorting%20the%20Summary%20Buffer]]
      gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-date)
      gnus-article-sort-functions
      '(gnus-article-sort-by-number
        gnus-article-sort-by-date))
