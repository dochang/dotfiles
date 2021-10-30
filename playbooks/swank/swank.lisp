(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors
   (load (merge-pathnames #P".common-lisp.lisp" (user-homedir-pathname)))
   (defparameter swank::*asdf-path*
     cl-user::*asdf-path*)))
