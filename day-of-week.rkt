#lang racket
(require rackunit)

; Month calculations
(define January 31)
(define February 28)
(define March 31)
(define April 30)
(define May 31)
(define June 30)
(define July 31)
(define August 31)
(define September 30)
(define October 31)
(define November 30)

(define (day-of-year-by-month month)
  (apply + (take (list January February March April May June July August September October November) (- month 1))))

(check-equal? (day-of-year-by-month 1) 0)
(check-equal? (day-of-year-by-month 2) 31)
(check-equal? (day-of-year-by-month 12) 334)


; Leap year calculations
(define leap-year-count-on-and-before-1752
  (+ (/ 1752 4)
     (- 0 (floor (/ 1752 100)))
     (floor (/ 1752 400))))

(check-equal? leap-year-count-on-and-before-1752 425)

(define (divisible-by dividend divisor)
  (= 0 (remainder dividend divisor)))

(define (is-leap-year year)
  (or (divisible-by year 400)
      (and (divisible-by year 4)
           (not (divisible-by year 100)))))

(define (leap-day-count year month)
  (+ (floor (/ year 4))
     (- (if (and (is-leap-year year) (< month 3)) 1 0 ))
     (- 0 (floor (/ year 100)))
     (floor (/ year 400))
     (- 0 leap-year-count-on-and-before-1752)))


(check-equal? (leap-day-count 1752 10) 0)
(check-equal? (leap-day-count 1753 1) 0)
(check-equal? (leap-day-count 1754 1) 0)
(check-equal? (leap-day-count 1755 1) 0)
(check-equal? (leap-day-count 1756 1) 0)
(check-equal? (leap-day-count 1756 2) 0)
(check-equal? (leap-day-count 1756 3) 1)

(define Sunday 1)
(define Monday 2)
(define Tuesday 3)
(define Wednesday 4)
(define Thursday 5)
(define Friday 6)
(define Saturday 7)

(define (to-day day-ordinal)
  (list-ref `(Sunday Monday Tuesday Wednesday Thursday Friday Saturday)
            (- day-ordinal 1)))

; Day calculations
; anchor date is Jan 1 1752
(define (day-of-week year month day)
  (let ([days-since-anchor (+
                            (* 365 (- year 1752))
                            (day-of-year-by-month month)
                            (- day 1)
                            (leap-day-count year month))])
    (to-day (+ 1 (remainder days-since-anchor 7)))))

; define some days


;handle by day
(check-equal? (day-of-week 1752 9 14) `Thursday)
(check-equal? (day-of-week 1752 9 24) `Sunday)
(check-equal? (day-of-week 1752 9 30) `Saturday)
(check-equal? (day-of-week 1752 10 1) `Sunday)
(check-equal? (day-of-week 1752 10 7) `Saturday)

;handle after first week
(check-equal? (day-of-week 1752 10 8) `Sunday)
(check-equal? (day-of-week 1752 10 31) `Tuesday)

;handle after month
(check-equal? (day-of-week 1752 11 1) `Wednesday)
(check-equal? (day-of-week 1752 12 31) `Sunday)

;handle after first year
(check-equal? (day-of-week 1753 1 1) `Monday)
(check-equal? (day-of-week 1753 7 28) `Saturday)
(check-equal? (day-of-week 1754 9 13) `Friday)
(check-equal? (day-of-week 1755 10 10) `Friday)
(check-equal? (day-of-week 1756 2 28) `Saturday)
(check-equal? (day-of-week 1756 2 29) `Sunday)

;handle leap year every 4 years
(check-equal? (day-of-week 1756 3 1) `Monday)
(check-equal? (day-of-week 1775 1 3) `Tuesday)
(check-equal? (day-of-week 1798 12 12) `Wednesday)
(check-equal? (day-of-week 1799 12 31) `Tuesday)
(check-equal? (day-of-week 1800 1 1) `Wednesday)
(check-equal? (day-of-week 1800 2 28) `Friday)

;handle not leap year every 100 years
(check-equal? (day-of-week 1800 3 1) `Saturday)
(check-equal? (day-of-week 1900 12 31) `Monday)
(check-equal? (day-of-week 2000 2 28) `Monday)

;is leap year when divisible by 400
(check-equal? (day-of-week 2000 2 28) `Monday)
(check-equal? (day-of-week 2000 3 1) `Wednesday)
(check-equal? (day-of-week 2019 10 28) `Monday)
(check-equal? (day-of-week 3568 5 12) `Sunday)

`(My birth: ,(day-of-week 1981 3 2))
`(Jens birth: ,(day-of-week 1981 5 18))
`(Lillian: ,(day-of-week 2015 10 16))
`(Evelyn: ,(day-of-week 2018 10 25))
`(Abe Lincoln: ,(day-of-week 1809 2 12))
`(Jean Luc Picard: ,(day-of-week 2305 7 13))
