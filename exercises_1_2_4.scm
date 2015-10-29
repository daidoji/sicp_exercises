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
        ((even? a) (fast-mult (halve a) (double b)))
        (else (+ b (fast-mult (- a 1) b)))))

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


;;; Exercise 1.19.  Define an iterative procedure running in O(ln N) for the
;;; fib sequence
;;; T_pq(n) -> 
    ;;; a <- bq + aq + ap and 
    ;;; b <- bp + aq
;;;
;;; T_01(n) ->
;;;     a <- b + a   ;;; n
;;;     b <- a       ;;; n - 1
;;;
;;; T_pq(n)^2 -> T_pq(T_pq(a,b)) ->
;;;     a' <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;;;     b' <- (bp + aq)p + (bq + aq + ap)q
;;;
;;; T_pq(n)^2 -> T_pq(T_pq(1,0)) -> (2,1)
;;;     a' <- (0p + 1q)q + (0q + 1q + 1p)q + (0q + 1q + 1p)p -> 2q^2 + 2pq + p^2
;;;     b' <- (0p + 1q)p + (0q + 1q + 1p)q -> 2pq + q^2
;;;
;;;     2 = 2q^2 + 2pq + p^2
;;;     1 = q^2 + 2pq
;;;
;;;     1 = q^2 + p^2 => sqrt(1 - q^2) = +p

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))      ; compute p'
                   (+ (* 2 p q) (* q q))    ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(fib 1) ;;; 1
(fib 2) ;;; 1
(fib 3) ;;; 2
(fib 4) ;;; 3
(fib 5) ;;; 5
(fib 6) ;;; 5
(fib 1000)
