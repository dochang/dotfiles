(req-package compile
  :config
  ;; Compilation for Gradle
  ;;
  ;; - [[http://kjeldahl.net/d7/node/34]]
  ;; - [[https://github.com/mariusk/android-with-emacs/blob/fb65f49666766e8c25b23b0377d086f6e55a3f5b/README.md]]
  (unless (assoc 'gradle compilation-error-regexp-alist-alist)
    (setq compilation-error-regexp-alist-alist
          (cons (list 'gradle ":compile.*?\\(/.*?\\):\\([0-9]+\\): " 1 2)
                compilation-error-regexp-alist-alist)))
  (add-to-list 'compilation-error-regexp-alist 'gradle))
