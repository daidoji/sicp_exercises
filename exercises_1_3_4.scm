;;; Exercise 1.40
;;;
;;; Define a procedure cubic that can use Newton's method to approximate cubics
;;; of the form x^3 + ax^2 + bx + c

;;; From text
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

(define (deriv g)
  (lambda (x) 
    (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;;; Cubic function defined
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(define (zero-cubic a b c)
  (newtons-method (cubic a b c) 1))
(zero-cubic 0 0 0)
(zero-cubic 0 0 1)

;;; Exercise 1.41
;;;
;;; Define a procedure double that takes a procedure of one argument as
;;; argument and returns a procedure that applies the original procedure twice.
(define (double procedure)
  (lambda (x) (procedure (procedure x))))
(define (inc x) (+ x 1))
((double inc) 5)
(((double (double double)) inc) 5)

;;; Exercise 1.42
;;;
;;; Let f and g be two one-argument functions. The composition f after g is
;;; defined to be the function x f(g(x)). Define a procedure compose that
;;; implements composition. For example, if inc is a procedure that adds 1 to
;;; its argument

(define (compose func_f func_g)
  (lambda (x) (func_f (func_g x))))

((compose square inc) 6)

;;; Exercise 1.43
;;;
;;;Write a procedure that takes as inputs a procedure that computes f and a
;;;positive integer n and returns the procedure that computes the nth repeated
;;;application of f. Your procedure should be able to be used as follows:

(define (repeated func iterations)
  (if (= iterations 0) 
    (lambda (x) (func x))
    (compose func (repeated func (- iterations 2)))))

((repeated square 2) 5)
