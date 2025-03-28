;;; Packages
(unless (alist-get 'package-archives **defaults**)
  (setf (alist-get 'package-archives **defaults**)
        (bound-and-true-p package-archives)))

(with-eval-after-load 'package
  (setopt package-archives
          (alist-get 'package-archives **globals**
                     (bound-and-true-p package-archives))))
