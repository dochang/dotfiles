(cl:in-package :cl-user)

;; hot-upgrade ASDF
(asdf:load-system :asdf)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
