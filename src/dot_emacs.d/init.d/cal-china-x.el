(req-package cal-china-x
  :disabled
  :init
  (setq calendar-holidays (append cal-china-x-chinese-holidays
                                  cal-china-x-important-holidays
                                  cal-china-x-general-holidays
                                  cal-china-x-japanese-holidays
                                  calendar-holidays)))
