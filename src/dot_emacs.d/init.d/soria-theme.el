(define-advice load-theme (:around (fn &rest r) purple-identifiers)
  (cond ((eq (car r) 'soria)
         (prog1
             (apply fn r)
           (add-hook 'prog-mode-hook 'soria-theme-purple-identifiers)))
        (t
         (progn
           (remove-hook 'prog-mode-hook 'soria-theme-purple-identifiers)
           (apply fn r)))))

(req-package soria-theme
  :init
  (let ((load-file-name (locate-library "soria-theme")))
    (when (and (boundp 'custom-theme-load-path) load-file-name)
      (add-to-list 'custom-theme-load-path
                   (file-name-as-directory (file-name-directory load-file-name)))))
  (setq **custom-themes** ($add-theme **custom-themes** 'soria)))
