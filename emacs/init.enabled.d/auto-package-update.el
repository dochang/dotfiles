;;; auto-package-update
(define-advice apu--packages-to-install (:filter-return (packages) delete-el-get-packages)
  "auto-package-update MUST NOT update the packages installed by el-get."
  (cl-set-difference packages el-get-activated-list))

(req-package auto-package-update)
