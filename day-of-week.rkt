#lang racket
(require rackunit)


; Month calculations

(define (day-of-year-by-month month acc)
  (match month
    [1 acc]
    [2 (day-of-year-by-month (- month 1) (+ acc 31))]
    [3 (day-of-year-by-month (- month 1) (+ acc 28))]
    [4 (day-of-year-by-month (- month 1) (+ acc 31))]
    [5 (day-of-year-by-month (- month 1) (+ acc 30))]
    [6 (day-of-year-by-month (- month 1) (+ acc 31))]
    [7 (day-of-year-by-month (- month 1) (+ acc 30))]
    [8 (day-of-year-by-month (- month 1) (+ acc 31))]
    [9 (day-of-year-by-month (- month 1) (+ acc 31))]
    [10 (day-of-year-by-month (- month 1) (+ acc 30))]
    [11 (day-of-year-by-month (- month 1) (+ acc 31))]
    [12 (day-of-year-by-month (- month 1) (+ acc 30))]))

(check-equal? (day-of-year-by-month 1 0) 0)
(check-equal? (day-of-year-by-month 2 0) 31)
(check-equal? (day-of-year-by-month 12 0) 334)


; Leap year calculations
(define leap-years-on-and-before-1752
  (/ 1752 4))

(define (is-leap-year year)
  (and
   (= 0 (remainder year 4))
   (> year 1752)))

(define (leap-day-count year month)
  (+ (floor (/ year 4))
     (- 0 leap-years-on-and-before-1752)
     (- (if (and (is-leap-year year) (< month 3)) 1 0 ))))

(check-equal? (leap-day-count 1752 1) 0)
(check-equal? (leap-day-count 1752 5) 0)
(check-equal? (leap-day-count 1753 1) 0)
(check-equal? (leap-day-count 1754 1) 0)
(check-equal? (leap-day-count 1755 1) 0)
(check-equal? (leap-day-count 1756 1) 0)
(check-equal? (leap-day-count 1756 2) 0)
(check-equal? (leap-day-count 1756 3) 1)

; Day calculations
(define (day-of-week year month day)
  (let ([days-since-anchor (+
                            (- day 1)
                            (day-of-year-by-month month 0)
                            (* 365 (- year 1752))
                            (leap-day-count year month))])
    (+ 1 (remainder days-since-anchor 7))))



;handle by day
(check-equal? (day-of-week 1752 9 14) 5)
(check-equal? (day-of-week 1752 9 24) 1)
(check-equal? (day-of-week 1752 9 30) 7)
(check-equal? (day-of-week 1752 10 1) 1)
(check-equal? (day-of-week 1752 10 7) 7)

;handle after first week
(check-equal? (day-of-week 1752 10 8) 1)
(check-equal? (day-of-week 1752 10 31) 3)

;handle after month
(check-equal? (day-of-week 1752 11 1) 4)
(check-equal? (day-of-week 1752 12 31) 1)

;handle after first year
(check-equal? (day-of-week 1753 1 1) 2)
(check-equal? (day-of-week 1753 7 28) 7)
(check-equal? (day-of-week 1754 9 13) 6)
(check-equal? (day-of-week 1755 10 10) 6)
(check-equal? (day-of-week 1756 2 28) 7)
(check-equal? (day-of-week 1756 2 29) 1)

;handle a leap year
(check-equal? (day-of-week 1756 3 1) 2)
(check-equal? (day-of-week 1775 1 3) 3)
(check-equal? (day-of-week 1798 12 12) 4)
(check-equal? (day-of-week 1799 12 31) 3)
(check-equal? (day-of-week 1800 1 1) 4)
(check-equal? (day-of-week 1800 2 28) 6)
