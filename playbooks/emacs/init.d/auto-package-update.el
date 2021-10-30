;;; auto-package-update

;; Consider the following packages up-to-date.
;;
;; 1. A non package.el package
;;
;;    This means `(package-installed-p PKG)' is nil.
;;
;; 2. A package that `package-archive-contents' doesn't have
;;
;;    This means `(cadr (assq PKG package-archive-contents))' is nil.
;;
;; 3. A package that `(package-desc-status PKG-DESC)' is not:
;;
;;    - unsigned
;;    - installed
;;    - dependency

(define-advice apu--package-up-to-date-p (:around (fn &rest r) pretest)
  (let ((pkg (car r)))
    (or (not (package-installed-p pkg))
        (null (cadr (assq pkg package-archive-contents)))
        (let* ((desc (or
                      (if (package-desc-p pkg) pkg)
                      (cadr (assq pkg package-alist))
                      (let ((built-in (assq pkg package--builtins)))
                        (if built-in
                            (package--from-builtin built-in)
                          (cadr (assq pkg package-archive-contents))))))
               (status (package-desc-status desc)))
          (not (member status '("unsigned" "installed" "dependency"))))
        (apply fn r))))

(req-package auto-package-update
  ;; :hook (emacs-startup . auto-package-update-maybe)
  ;;
  ;; Don't run `auto-package-update-maybe' at startup.  I don't want to wait
  ;; for it at startup.
  :init
  (setq auto-package-update-delete-old-versions t))
