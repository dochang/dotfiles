;;; Packages
(setf (alist-get 'package-archives **defaults**)
      (bound-and-true-p package-archives))

(setq package-archives
      (alist-get 'package-archives **globals**
                 (bound-and-true-p package-archives)))
