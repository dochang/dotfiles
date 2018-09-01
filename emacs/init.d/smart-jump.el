(defun $smart-jump-go-other-window (&optional smart-list continue)
  (interactive)
  (pop-to-buffer (current-buffer) t)
  (smart-jump-go smart-list continue))

(req-package smart-jump
  :bind (:map esc-map
         ("." . smart-jump-go)
         ("," . smart-jump-back)
         ("?" . smart-jump-references)
         :map ctl-x-4-map
         ("." . $smart-jump-go-other-window)
         :map ctl-x-5-map
         ("." . smart-jump-peek))
  :config
  (smart-jump-setup-default-registers))
