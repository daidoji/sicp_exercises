; Exercise 1.29
; Simpson's Rule is a more accurate method of numerical integration than the
; method illustrated above. Using Simpson's Rule, the integral of a function f
; between a and b is approximated as

; (h/3)[y_0 + 4 * y_1 + 2 * y_2 + 4 * y_3 + 2 * y_4 ... + 2 * y_n-2 + 4 * y_n-1 + y_n]

; where h = (b - a)/n, for some even integer n, and yk = f(a + kh). (Increasing
; n increases the accuracy of the approximation.) Define a procedure that takes
; as arguments f, a, b, and n and returns the value of the integral, computed
; using Simpson's Rule. Use your procedure to integrate cube between 0 and 1
; (with n = 100 and n = 1000), and compare the results to those of the integral
; procedure shown above. 

(define (sum term a next b) 
  (if (> a b) 
    0 
    (+ (term a) (sum term (next a) next b))
  )
)

(define (inc n) (+ n 1))

(define (simpsons-integral func a b n)
  (define (h) (/ (- b a) n))
  (define (multiple k)
    (cond ((or (= k 0) (= k n)) 1)
          ((= (modulo k 2) 0) 2)
          (else 4)))
  (define (term k) (* (multiple k) (func (+ a (* k (h))))))
  (* (/ (h) 3)
     (sum term 0 inc n)
  )
)

(define (cube x) (* x x x))

(simpsons-integral cube 0.0 1 100)
(simpsons-integral cube 0.0 1 1000)

; Exercise 1.30
; Create an iterative sum procedure below

(define (iter-sum term a next b)
  (define (iter a result)
    (if (> a b) result 
      (iter (next a) (+ (term a) result))))
  (iter 1 0))

(sum cube 1 inc 10)
(iter-sum cube 1 inc 3)
