;;; Browse URL

(defmacro $browse-url-define-incognito-function (browser-name browser-args)
  (declare (indent nil))
  (let ((fname (intern (format "$%s/incognito" browser-name))))
    `(defun ,fname (url &optional new-window)
       (interactive (browse-url-interactive-arg "URL: "))
       (let ((,browser-args
              (if (member "--incognito" ,browser-args)
                  ,browser-args
                (cons "--incognito" ,browser-args))))
         (,browser-name url new-window)))))

($browse-url-define-incognito-function browse-url-chromium
                                       browse-url-chromium-arguments)

($browse-url-define-incognito-function browse-url-chrome
                                       browse-url-chrome-arguments)

(defun $browse-url-sensible-browser (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((browse-url-generic-program "sensible-browser"))
    (browse-url-generic url new-window)))

(defun $browse-url-clipboard (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (with-temp-buffer
    (insert url)
    (copy-region-as-kill (point-min) (point-max))))

(defun $browse-url-default-browser (url &rest args)
  (interactive (browse-url-interactive-arg "URL: "))
  (let* ((browser-alist '(("firefox" . browse-url-firefox)
                          ("chromium" . browse-url-chromium)
                          ("chromium/incognito" . $browse-url-chromium/incognito)
                          ("sensible-browser" . $browse-url-sensible-browser)
                          ("clipboard" . $browse-url-clipboard)))
         (browser (completing-read "Browser: " browser-alist
                                   nil t nil nil "clipboard"))
         (browser-function (cdr (assoc browser browser-alist))))
    (apply browser-function url args)))

(req-package browse-url
  :commands (browse-url-interactive-arg)
  :custom
  (browse-url-browser-function '$browse-url-default-browser))
