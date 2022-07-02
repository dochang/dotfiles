;;; Theme
(defvar **theme-engine**
  (cond ((>= emacs-major-version 24) 'custom-theme)
        ((>= emacs-major-version 21) 'color-theme)))

(setq custom-theme-directory (locate-user-emacs-file "themes/"))

(defvar **custom-themes** '())

(defvar **color-themes** '())

(defun $add-theme (themes theme &optional pred override)
  (let ((entry (assq theme themes)))
    (cond ((and entry override)
           (cons (cons theme pred)
                 (assq-delete-all theme themes)))
          ((null entry)
           (cons (cons theme pred) themes)))))

(defun $theme-list (&optional display)
  (seq-reduce
   (lambda (result entry)
     (let* ((theme (car entry))
            (pred (cdr entry))
            (match (cond ((null pred)
                          t)
                         ((consp pred)
                          (memq (framep-on-display display) pred))
                         (t
                          (funcall pred display)))))
       (if match
           (cons theme result)
         result)))
   (cond ((eq **theme-engine** 'custom-theme)
          **custom-themes**)
         ((eq **theme-engine** 'color-theme)
          **color-themes**)
         (t '()))
   '()))

(defvar **theme-initialized** nil)

(defvar color-theme-is-cumulative)
(defun $theme-initialize ()
  (unless **theme-initialized**
    (cond ((eq **theme-engine** 'custom-theme)
           ;; `custom-theme' has been initialized during startup (See
           ;; `lisp/loadup.el`).
           (setq **theme-initialized** t))
          ((eq **theme-engine** 'color-theme)
           (when (require 'color-theme nil t)
             ;; The docstring of `color-theme-is-cumulative' is wrong.
             ;;
             ;; Installing a new theme will undo all settings only if *nil*.
             ;;
             ;; http://www.emacswiki.org/emacs/ColorThemeQuestions#toc9
             ;; http://savannah.nongnu.org/bugs/?29500
             (setq color-theme-is-cumulative nil)
             ;; The variable `color-theme-initialized' is not used in
             ;; `color-theme'.  There is no way to determine whether
             ;; `color-theme` has been initialized.
             (setq **theme-initialized** t))))))

(defun $random-elt (sequence)
  (nth (random (length sequence)) sequence))

(defvar **theme-selector** '$random-elt)

(defun $select-theme (&optional display)
  (funcall **theme-selector** ($theme-list display)))

(defvar **theme** nil)

;; **TODO** If emacs is running in daemon mode, setting a theme in a frame will
;; cause the font in current frame become very small.  We have to create a new
;; frame and close the old one.
;;
;; If there have been already 2 frames before setting theme, the font in
;; current frame will not change.
(defvar color-themes)
(defun $set-theme (&optional theme)
  (interactive
   (list
    (let ((input (completing-read
                  "Theme (leave blank or `nil' to select one randomly): "
                  (mapcar 'symbol-name
                          (delete-dups
                           (append
                            ($theme-list)
                            (cond ((eq **theme-engine** 'custom-theme)
                                   (custom-available-themes))
                                  ((eq **theme-engine** 'color-theme)
                                   color-themes)
                                  (t '()))
                            '()))))))
      (if (equal "" input) nil (intern input)))))
  (when (and **theme-initialized**
             (setq theme (or theme ($select-theme))))
    (message "Theme: %s" theme)
    (cond ((eq **theme-engine** 'custom-theme)
           (mapc 'disable-theme (copy-sequence custom-enabled-themes))
           (when (load-theme theme t)
             (setq **theme** theme)))
          ((and (eq **theme-engine** 'color-theme)
                (featurep 'color-theme))
           ;; The first theme should not undo the original settings.
           (let ((color-theme-is-cumulative (null **theme**)))
             (funcall theme))
           (setq **theme** theme)))))
