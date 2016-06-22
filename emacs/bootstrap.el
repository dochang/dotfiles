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
            el-get-install-skip-emacswiki-recipes
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
  (unless (assq 'mb-url package-archive-contents)
    (package-refresh-contents))
  (package-install 'mb-url))
;; If mb-url has not been installed, I believe the archive contents are empty.
;; If mb-url cannot be installed, clear the archive.

;; Delete el-get bootstrap mb-url
(when (el-get-package-installed-p 'mb-url)
  (el-get-remove 'mb-url))
