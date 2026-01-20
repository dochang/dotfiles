(defun $disable-bg-color-in-terminal (frame)
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))
;; https://stackoverflow.com/a/20233611
;;
;; The `background-mode' of a text-only terminal frame (a termcap frame or a
;; direct-write MS-DOS frame) reports the `background-mode' of the terminal
;; (which usually means `dark'), even if emacs is in a light theme.
;;
;; Manually disable background color in a terminal frame.
;;
;; See also:
;;
;; - `display-graphic-p'
;; - `framep'
;; - `frame-live-p'

(defun $rand-theme ()
  (rand-theme)
  (mapc '$disable-bg-color-in-terminal (frame-list)))

(defvar **rand-theme-try-dark-threshold** 8)

(defun $dark-theme-p ()
  (let ((graphic-frame (if (display-graphic-p (selected-frame))
                           (selected-frame)
                         (seq-find 'display-graphic-p (frame-list) (selected-frame)))))
    (eq (frame-parameter graphic-frame 'background-mode) 'dark)))
;; How is a frame's `background-mode' determined?
;;
;; Function `frame-terminal-default-bg-mode' returns default
;; background mode of a frame.
;;
;; Function `frame-set-background-mode' sets the actual
;; `background-mode' of a frame.

(defun $set-theme (&optional theme)
  (interactive
   (list
    (let ((input (completing-read
                  "Theme (leave blank or `nil' to select one randomly): "
                  (mapcar 'symbol-name (custom-available-themes)))))
      (if (equal "" input) nil (intern input)))))
  (if theme
      (let ((rand-theme-wanted (list theme)))
        ($rand-theme))
    ($rand-theme)
    (let ((i 0))
      (while (and (or (null **rand-theme-try-dark-threshold**)
                      (< i **rand-theme-try-dark-threshold**))
                  (not ($dark-theme-p)))
        ($rand-theme)
        (setq i (+ i 1))))))

(setup (:package rand-theme)
  (add-hook 'emacs-startup-hook #'$set-theme)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (unless ($dark-theme-p)
                ($set-theme))
              ($disable-bg-color-in-terminal frame)))
  ;; For the case that Emacs starts in terminal or daemon mode.

  (:when-loaded

    (setopt rand-theme-unwanted '(nil))
    ;; Either `rand-theme-unwanted' or `rand-theme-wanted' needs to be set to
    ;; non-nil.

    )

  )
