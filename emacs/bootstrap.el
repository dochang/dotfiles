(load (locate-user-emacs-file "packages"))

(unless (require 'el-get nil 'noerror)
  (with-temp-buffer
    (with-current-buffer
        (let* ((url "https://raw.github.com/dimitri/el-get/master/el-get-install.el"))
          (cond ((executable-find "curl")
                 (call-process "curl" nil t nil "--silent" "--show-error" "--location" url)
                 (current-buffer))
                ((executable-find "wget")
                 (call-process "wget" nil t nil "--quiet" "--output-document" "-" url)
                 (current-buffer))
                (t
                 (url-retrieve-synchronously url))))
      (let (el-get-master-branch
            (el-get-git-install-url (getenv "EL_GET_GIT_INSTALL_URL")))
        (when (and (stringp el-get-git-install-url)
                   (string= "" el-get-git-install-url))
          (setq el-get-git-install-url nil))
        (goto-char (point-max))
        (eval-print-last-sexp)))))

(el-get)

(unless (require 'package nil 'noerror)
  (el-get 'sync 'package))

(setq package-enable-at-startup nil)
(package-initialize)

(or (require 'mb-url nil 'noerror)
    (el-get 'sync 'mb-url)
    (el-get-bundle! mb-url in dochang/mb-url
      :branch "master"
      :depends ()
      :library mb-url))

(advice-add 'url-http :override 'mb-url-http-curl)

(unless (package-installed-p 'mb-url)
  (package-install 'mb-url))
;; If the archive contents are empty, there is no need to call
;; `package-refresh-contents'.  `package-install' will refresh the archive
;; contents if empty.
;;
;; If mb-url has been installed, I believe the archive contents are not
;; empty.  If there are anything wrong, clear the archive.

;; Delete el-get bootstrap mb-url
(when (el-get-package-installed-p 'mb-url)
  (el-get-remove 'mb-url))