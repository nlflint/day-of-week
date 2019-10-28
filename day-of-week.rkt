#lang racket
(require rackunit)


(define (day-of-week year month day)
  (+ 1 (remainder (- day 1) 7)))

; Sunday October 1, 1752 is anchor

(check-equal? (day-of-week 1752 10 1) 1)
(check-equal? (day-of-week 1752 10 7) 7)
(check-equal? (day-of-week 1752 10 8) 1)
(check-equal? (day-of-week 1752 10 31) 3)