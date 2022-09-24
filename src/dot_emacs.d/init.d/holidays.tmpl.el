#jinja2: trim_blocks: "true", lstrip_blocks: "false"
(req-package holidays
  :ensure (holidays :pin :built-in)
  :init
  (setq holiday-other-holidays
        '(
          {% for holiday in dotfiles_emacs_holiday_other_holidays %}
          {{ holiday }}
          {% endfor %}
          )))
