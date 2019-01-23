(req-package holidays
  :ensure (holidays :pin :built-in)
  :custom
  (holiday-other-holidays
   '(
     {% for holiday in dotfiles_emacs_holiday_other_holidays %}
     {{ holiday }}
     {% endfor %}
     )))
