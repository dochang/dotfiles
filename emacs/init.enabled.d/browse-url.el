;;; Browse URL

(defun $browse-url-conkeror (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((browse-url-generic-program "conkeror")
        (browse-url-generic-args nil))
    (browse-url-generic url new-window)))

(defun $browse-url-chromium (url &optional new-window)
  "Ask the Chromium WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-chromium-arguments' are also passed to
Chromium."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "chromium " url) nil
           (or (bound-and-true-p browse-url-chromium-program) "chromium")
           (append
            (bound-and-true-p browse-url-chromium-arguments)
            (list url)))))

(defun $browse-url-chromium/incognito (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((browse-url-chromium-arguments
         (cons "--incognito" (bound-and-true-p browse-url-chromium-arguments))))
    (cond ((<= emacs-major-version 23)
           ($browse-url-chromium url new-window))
          (t
           (browse-url-chromium url new-window)))))

(defun $browse-url-sensible-browser (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((browse-url-generic-program "sensible-browser"))
    (browse-url-generic url new-window)))

(defun $browse-url-clipboard (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (with-temp-buffer
    (insert url)
    (copy-region-as-kill (point-min) (point-max))))

(defun $browse-url-default-browser (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((browse-url-generic-program
         (or (executable-find "xdg-open")
             (executable-find "sensible-browser"))))
    (browse-url-generic url new-window)))

(req-package browse-url
  :commands (browse-url-interactive-arg)
  :init
  (setq browse-url-browser-function '$browse-url-default-browser))
