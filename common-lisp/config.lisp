;; -*- mode: lisp -*-
(in-package :cl-user)

#+ccl
(setf ccl:*default-file-character-encoding* :utf-8
      ccl:*default-socket-character-encoding* :utf-8)
;; https://ccl.clozure.com/manual/chapter4.5.html#External-Formats
;; https://lists.clozure.com/pipermail/openmcl-devel/2013-October/010328.html
;; http://zoomq.qiniudn.com/ZQScrapBook/ZqFLOSS/data/20120220184701/index.html

(ignore-errors (require :asdf))

#-asdf2
(load (make-pathname :name "asdf" :type "lisp" :defaults *load-truename*))

;; hot-upgrade ASDF
(asdf:load-system :asdf)

#+(or clisp cmu)
(progn
  (defun sysdef-find-clx (name)
    (let* ((x #+clisp (asdf::find-symbol* '#:*module-provider-functions* :custom)
              #+cmu 'ext:*module-provider-functions*)
           (syms (and x (list x)))
           (vals (and x (list (remove 'asdf::module-provide-asdf (symbol-value x))))))
      (progv syms vals
        (require #+clisp "clx" #+cmu :clx)))
    (asdf::find-system-fallback name "clx"))
  (pushnew 'sysdef-find-clx asdf:*system-definition-search-functions*))
