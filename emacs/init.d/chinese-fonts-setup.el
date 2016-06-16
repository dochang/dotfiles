;; chinese-fonts-setup
;;
;; [[http://www.newsmth.net/nForum/#!article/Emacs/108473]]
;; [[https://github.com/tumashu/chinese-fonts-setup]]
;; [[http://baohaojun.github.io/perfect-emacs-chinese-font.html]]
;; [[http://zhuoqiang.me/torture-emacs.html]]
(req-package chinese-fonts-setup
  :commands (cfs-edit-profile
             cfs-regenerate-profile
             cfs-switch-profile
             cfs-next-profile
             cfs-increase-fontsize
             cfs-decrease-fontsize)
  :init
  (setq cfs-profiles '("default")))
