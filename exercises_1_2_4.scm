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

;;; Exercise 1.17 Suppose we have addition, an operation 'halve' which divides
;;; an even integer by 2, and 'double' which doubles an integer.  Design an
;;; iterative multiplication procedure that uses a logarithmic number of steps.
;;;
;;; 8 5  5
;;; 4 10 10
;;; 2 20 20
;;; 1 40 40
;;;
;;; 5 3
;;; 4 6
;;; 2 12
;;; 1 15

(define (halve int)
  (if (even? int) (/ int 2)))

(define (double int) (+ int int))

(define (mult a b) (fast-mult a b))

(define (fast-mult a b) 
  (cond ((= a 0) 0)
        ((= a 1) b)
        ((even? a) (fast-mult-iter a b b))
        (else (fast-mult-iter a b 0))))

(define (fast-mult-iter a b tot)
  (cond ((= a 1) (+ tot b))
        ((even? a) (fast-mult-iter (halve a) b (double tot)))
        (else (fast-mult-iter (+ a -1) b (+ tot b)))))


(mult 2 3)
(* 2 3)
(mult 3 4)
(* 3 4)
(mult 5 6)
(* 5 6)
(mult 8 7)
(* 8 7)
(mult 9 10)
(* 9 10)
(mult 12 11)
(* 12 11)
