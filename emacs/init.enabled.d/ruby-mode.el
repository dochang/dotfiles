;;; Ruby Mode

(defun $ruby-mode-hook ()
  ($run-prog-mode-hook)
  (inf-ruby-minor-mode)
  (ruby-block-mode 1))

(req-package ruby-mode
  :mode (;; For Gem
         ("\\.gemspec\\'" . ruby-mode)
         ;; For Bundler
         ("Gemfile\\'" . ruby-mode)
         ;; For Rack
         ("config\\.ru\\'" . ruby-mode)
         ;; For Rake
         ("\\.rake\\'" . ruby-mode))
  :hook (ruby-mode . $ruby-mode-hook))
