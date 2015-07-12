;;; Exercise 1.11 Define the folling function recursively and iteratively.
;;; f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3

;;; Recursive
(define (f n)
  (cond ((< n 3) n)
	    (else (+ (f (- n 1)) 
                 (* (f (- n 2)) 2)
                 (* (f (- n 3)) 3)))))

(f 5)

;;; Iterative
(define (f n)
  (f-iter n 3 2 1 0))

(define (f-iter n count fn-one fn-two fn-three)
  (cond ((< n 3) n)
        ((= n count) (+ fn-one (* 2 fn-two) (* 3 fn-three)))
        (else (f-iter n
                      (+ count 1)
                      (+ fn-one (* 2 fn-two) (* 3 fn-three))
					  fn-one
                      fn-two))))

(f 5)

;;; Exercise 1.12 Define function to compute element of Pascal's triangle
(define (pascal-element depth position)
  (if (or (= depth 1) (= position 1) (= position depth)) 
      1
	  (+ (pascal-element (- depth 1) (- position 1))
		 (pascal-element (- depth 1) position))))
        
(pascal-element 5 2)
