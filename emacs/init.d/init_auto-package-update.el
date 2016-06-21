;;; auto-package-update
(defun $apu--packages-to-install-delete-el-get-packages (packages)
  "auto-package-update MUST NOT update the packages installed by el-get."
  (cl-set-difference packages el-get-activated-list))

(req-package auto-package-update
  :init
  (advice-add 'apu--packages-to-install :filter-return
              '$apu--packages-to-install-delete-el-get-packages))
