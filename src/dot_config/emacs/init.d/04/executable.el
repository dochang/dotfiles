(defun $executable-make-buffer-file-executable-if-script-p ()
  (when (derived-mode-p 'prog-mode)
    (executable-make-buffer-file-executable-if-script-p)))

(setup (:package executable)

  (add-hook 'after-save-hook #'$executable-make-buffer-file-executable-if-script-p)

  )
