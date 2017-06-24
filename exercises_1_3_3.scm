;;; Exercise 1.35 
;;;
;;; Show that the golden ratio is a fixed point of x -> 1 + 1/x

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

;;; Exercise 1.36
;;;
;;; Change fixed-point to display iterations.  Then find a solution to 
;;; x^x == 1000 via fixed point of x <- log(1000)/log(x)

;;; With average damping
(define (print-iter-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try (/ (+ next guess) 2))))) ;;; dampen here
  (try first-guess))
(print-iter-fixed-point (lambda (x) (/ (log 1000) (log x))) 100.0)

;;; Without average damping.
(define (print-iter-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next)))) ;;; just get the next one
  (try first-guess))
(print-iter-fixed-point (lambda (x) (/ (log 1000) (log x))) 100.0)

;;; Exercise 1.37
;;;
;;; Write an iterative and recursive procedure for computing a k-term finite
;;; continued fraction (continued fraction truncated at some point).  Check by
;;; correctly evaluting (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k) which
;;; should converge to 1/φ = 0.61803398875

;;; recursive
(define (cont-frac n d max_iterations)
  (define (cont-frac-recur iterations)
    (if (< iterations max_iterations)
      (/ (n iterations) (+ (d iterations) (cont-frac-recur (+ iterations 1))))
      (/ (n iterations) (d iterations))
    )
  )
  (cont-frac-recur 1))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 50)

;;; iterative
(define (cont-frac n d max_iterations)
  (define (cont-frac-iter iterations accumulator)
    (if (= iterations 0)
      accumulator
      (cont-frac-iter (- iterations 1) (/ (n iterations) (+ (d iterations) accumulator)))
    ))
  (cont-frac-iter max_iterations (/ (n max_iterations) (d max_iterations))))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 50)

;;; Exercise 1.38
;;;
;;; Use cont-frac procedure to approximate e using Euler's expansion.  Euler's
;;; expansion is a k-term continued fraction where D_i == 1 and N_i is the
;;; sequence 1, 2, 1, 1, 4, 1, 1, 6...
;;; e = 2.71828182845904
;;; e - 2 = 0.71828182845904

(define (seq i)
  (define (seq-accum step accumulated)
    (if (= i step)
      (if (= (modulo step 3) 2) (+ accumulated 2) 1)
      (if (= (modulo step 3) 2)
        (seq-accum (+ step 1) (+ accumulated 2))
        (seq-accum (+ step 1) accumulated))
  ))
  (seq-accum 1 0))
(cont-frac (lambda (i) 1.0) seq 100)

;;; Exercise 1.39
;;; 
;;; Implement cont-frac implementation of Lambert's tangent function

(define (seed-n x)
  (lambda (i)
    (if (= i 1)
      x
      (- (* x x)))))

(define (d i)
  (+ 1 (* (- i 1) 2)))

(tan 2.0)
(cont-frac (seed-n 2.0) d 10)
