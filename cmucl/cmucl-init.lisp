(cl:in-package :cl-user)

;; Bootstrap ASDF
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors (require "asdf"))
  (unless (member :asdf3.2 *features*)
    (load (merge-pathnames #P".common-lisp.lisp" (user-homedir-pathname)))
    (load *asdf-path*)))

(load (merge-pathnames #P"common-lisp/config.lisp" (uiop:xdg-config-home)))
