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
    (display elapsed-time)
    elapsed-time)

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
;;; refine procedure so that it runs much faster than original one due to fact
;;; that after we've checked 2 we no longer need to check all the evens.

(define (find-divisor n test-divisor) 
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor) 
    (else (find-divisor n (+ test-divisor 1)))))

(search-for-primes 1001 1100)
(search-for-primes 10001 10100)
(search-for-primes 100001 100100)
(search-for-primes 1000001 1000100)

;;; Exercise 1.24
;;; 
;;; we modify the timed-prime-test to use the probalistic fermat method instead
;;; of a deterministic sieve and evalute the time of some of the primes we
;;; found in exercise 1.22

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) 
         (remainder (square (expmod base (/ exp 2) m))
                     m))
        (else 
          (remainder (* base (expmod base (- exp 1) m))
                      m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
    (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))

(define (start-prime-test n start-time)
    (if (fast-prime? n 10000) (report-prime (- (runtime) start-time))))

(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)
(timed-prime-test 1000039)
(timed-prime-test 1000081)
(timed-prime-test 1000099)

;;; Excercise 1.27
;;;
;;; Demonstrate the Carmicheal numbers 561, 1105, 1729, 2465, 2821, and 6601
;;; fool the fermat-test.  Then write a procedure that takes an integer n and
;;; tests whether a^n is congruent to a mod n for all a < n and try that
;;; function on the Carmicheal numbers

(fermat-test 4)
(fermat-test 561)
(fermat-test 1105)
(fermat-test 1729)
(fermat-test 2465)
(fermat-test 2821)
(fermat-test 6601)
