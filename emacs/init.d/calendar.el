;;; Calendar

(defun $diary-chinese-date (month day &optional year mark)
  "Specific date(s) diary entry.
Entry applies if date is MONTH, DAY, YEAR.  Each parameter can be a
list of integers, t (meaning all values), or an integer.  The order
of the input parameters changes according to `calendar-date-style'
\(e.g. to DAY MONTH YEAR in the European style).

YEAR can also be nil, which means all values like t.

An optional parameter MARK specifies a face or single-character string
to use when highlighting the day in the calendar."
  ;; Load `cal-china' for `calendar-chinese-from-absolute'.
  (when (require 'cal-china nil t)
    (pcase-let* ((ddate (diary-make-date month day year))
                 (`(,dy ,dm ,dd)
                  (list
                   (calendar-extract-year ddate)
                   (calendar-extract-month ddate)
                   (calendar-extract-day ddate)))
                 (`(,cc ,cy ,m ,d)      ; current chinese date
                  (calendar-chinese-from-absolute
                   (calendar-absolute-from-gregorian
                    (or date (calendar-current-date)))))
                 (y (+ (* cc 100) cy))
                 ;; The chinese year is (+ (* cc 100) cy) actually.
                 ;; See `calendar-chinese-from-absolute-for-diary' for details.
                 )
      (and
       (or (and (listp dd) (memql d dd))
           (equal d dd)
           (eq dd t))
       (or (and (listp dm) (memql m dm))
           (equal m dm)
           (eq dm t))
       (or (not dy)
           (and (listp dy) (memql y dy))
           (equal y dy)
           (eq dy t))
       (cons mark entry)))))

(defun $diary-chinese-hair-cutting-date (day)
  (or ($diary-chinese-date '(1 1.5) '())
      ($diary-chinese-date 2 (list 8 day))
      ;; 8 = 2 + 6.  Must cut hair in [2-2, 2-6]. My hair will be too long
      ;; after 2-7.
      ($diary-chinese-date (number-sequence 2.5 12.5 0.5) day)))

(req-package calendar
  :custom
  (calendar-chinese-all-holidays-flag t)
  (calendar-mark-holidays-flag t)
  (calendar-week-start-day 1))
