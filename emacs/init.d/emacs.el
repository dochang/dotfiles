(defun $emacs-startup-hook ()
  (unless **theme-initialized**
    ($theme-initialize))
  ($set-theme))

(req-package emacs

  :hook ((emacs-startup . $emacs-startup-hook))

  :init

  (setq user-full-name (or (getenv "NAME") "{{ dotfiles_emacs_name }}"))
  (setq user-mail-address (or (getenv "EMAIL") "{{ dotfiles_emacs_email }}"))

  ;; Disable auto save recovery record.
  ;; [[info:emacs#Recover]]
  (setq auto-save-list-file-prefix nil)

  ;; Set fill column to 79
  ;;
  ;; A line begins at column 0 in Emacs.  79 is the last column on an 80-width
  ;; screen.  Do not occupy column 79, leave it for filling.
  ;;
  ;; This setting will make Emacs fill the paragraphs at the bottom as is.
  (setq-default fill-column 79)

  ;; Disable `indent-tabs-mode'.
  ;;
  ;; `web-mode' changes indentation settings if `indent-tabs-mode' is non-nil.
  ;; To prevent it, set the default value of `indent-tabs-mode' to `nil'.
  (setq-default indent-tabs-mode nil)

  ;; Keep point at the same screen position after scrolling.
  ;; [[info:emacs#Scrolling]]
  (setq scroll-preserve-screen-position t)

  ;; UI Configuration
  (setq-default cursor-type 'bar)
  (setq-default cursor-in-non-selected-windows nil)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default indicate-empty-lines t)

  (setq visible-bell t)
  (setq inhibit-startup-screen t)

  )

;; ----------------------------------------------------------------------------

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;; ----------------------------------------------------------------------------
