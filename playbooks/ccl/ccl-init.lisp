(cl:in-package :cl-user)

;; Bootstrap ASDF
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf ccl:*default-file-character-encoding* :utf-8
        ccl:*default-socket-character-encoding* :utf-8)
  ;; https://ccl.clozure.com/manual/chapter4.5.html#External-Formats
  ;; https://lists.clozure.com/pipermail/openmcl-devel/2013-October/010328.html
  ;; http://zoomq.qiniudn.com/ZQScrapBook/ZqFLOSS/data/20120220184701/index.html

  (ignore-errors (require "asdf"))
  (unless (member :asdf3.2 *features*)
    (load (merge-pathnames #P".common-lisp.lisp" (user-homedir-pathname)))
    (load *asdf-path*)))

(load (merge-pathnames #P"common-lisp/config.lisp" (uiop:xdg-config-home)))
