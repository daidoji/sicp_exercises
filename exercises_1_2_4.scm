;;; Exercise 1.16 Define a process that uses an iterative exponential
;;; process that uses successive squaring with a logarithmic number of
;;; steps.

(define (fast-expt-iter b n a) 
  (cond ((= n 1) a)
        ((even? n) (fast-expt-iter b (/ n 2) (* (square b) a)))
        (else (fast-expt-iter b (- n 1) (* b a)))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((= n 1) b)
        (else (fast-expt-iter b n 1))))

(fast-expt 2 5)
