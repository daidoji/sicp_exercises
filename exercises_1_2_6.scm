;;; Exercise 1.21 Find divsors for 199, 1999, 199999
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor) 
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor) 
    (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 199999)

;;; Exercise 1.22
;;;
;;; This exercise is to show a slowdown proportional to the big-O time
;;; discussed in the text.  On my i7 processor with tons of ram using Release
;;; 9.0.1 of MIT Scheme most of these tests are far too slow to notice any
;;; difference in these orders of magnitude.

(define (smallest-divisor n)
    (find-divisor n 2))
(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
    (= (remainder b a) 0))

(define (prime? n)
    (= n (smallest-divisor n)))

(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
    (display " *** ")
      (display elapsed-time))

(define (search-for-primes a b)
  (cond ((>= a b) a)
        ((even? a) 
         (timed-prime-test (+ a 1)) 
         (search-for-primes (+ a 3) b))
        (else
         (timed-prime-test a)
         (search-for-primes (+ a 2) b))))

(search-for-primes 1001 1100)
(search-for-primes 10001 10100)
(search-for-primes 100001 100100)
(search-for-primes 1000001 1000100)

;;; Exercise 1.23
;;; b
(define (next input)
  (if (= (- input 2) 0) 3 (+ input 2)))

(define (smallest-divisor n)
    (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
    (= (remainder b a) 0))

(timed-prime-test 199)
(timed-prime-test 19999)
(timed-prime-test 199999)
