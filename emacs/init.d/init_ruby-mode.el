;;; Ruby Mode

(defun $ruby-mode-hook ()
  ($prog-mode-hook*)
  (local-set-key "\C-\M-h" 'ruby-mark-defun)
  (inf-ruby-minor-mode)
  (ruby-block-mode 1))

(req-package ruby-mode
  :loader :built-in
  :mode (;; For Gem
         ("\\.gemspec\\'" . ruby-mode)
         ;; For Bundler
         ("Gemfile\\'" . ruby-mode)
         ;; For Rack
         ("config\\.ru\\'" . ruby-mode)
         ;; For Rake
         ("\\.rake\\'" . ruby-mode))
  :init
  (add-hook 'ruby-mode-hook '$ruby-mode-hook))
