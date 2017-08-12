(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *asdf-path*
    (make-pathname :name "asdf" :type "lisp"
                   :defaults (or *compile-file-pathname* *load-pathname*))
    "Local path of `asdf.lisp`.  This local copy comes from [1].

[1]: https://common-lisp.net/project/asdf/archives/asdf.lisp"))
