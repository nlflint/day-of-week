#lang racket
(require rackunit)


; Sunday October 1, 1752 is anchor

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

(define (day-of-week year month day)
  (let ([days-since-anchor (+
                            (- day 1)
                            (day-of-year-by-month month 0)
                            (* 365 (- year 1752)))])
    (+ 1 (remainder days-since-anchor 7))))



;handle by day
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
