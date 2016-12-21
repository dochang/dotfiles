(defmacro $set-homebrew-var (var command name)
  (let ((proc (make-symbol "proc"))
        (evt (make-symbol "evt")))
    `(make-process
      :name ,name
      :buffer (generate-new-buffer ,name)
      :command ,command
      :connection-type 'pipe
      :sentinel (lambda (,proc ,evt)
                  (when (string= ,evt "finished\n")
                    (unwind-protect
                        (setq ,var
                              (with-current-buffer (process-buffer ,proc)
                                (buffer-substring
                                 (point-min)
                                 (save-excursion
                                   (goto-char (point-min))
                                   (re-search-forward "[^\r\n]*" nil t)
                                   (point)))))
                      (kill-buffer (process-buffer ,proc))))))))

(req-package homebrew-mode
  :init
  (cond ((executable-find "brew")
         ;; Do not use `shell-command-to-string' here.  Homebrew and Linuxbrew
         ;; are written in Ruby and sometimes Ruby is a bit slow.  We have to
         ;; do asynchronous operations.
         ($set-homebrew-var
          homebrew-prefix '("brew" "--prefix") "brew--prefix")
         ($set-homebrew-var
          homebrew-cache-dir '("brew" "--cache") "brew--cache"))
        ((eq system-type 'gnu/linux)
         (setq homebrew-prefix "~/.linuxbrew")
         (setq homebrew-cache-dir "~/.cache/Homebrew"))
        ((eq system-type 'darwin)
         (setq homebrew-prefix "/usr/local")
         (setq homebrew-cache-dir "~/Library/Caches/Homebrew/"))))
