;;; Symbolic differentiation, adapted from chapter 2.3.2
;;; "Structure and Interpretation of Computer Programs".

(define variable?
  (lambda (x)
    (symbol? x)))

(define same-variable?
  (lambda (v1 v2)
    (if (and (variable? v1) (variable? v2))
	(eq? v1 v2)
	#f)))

(define =number?
  (lambda (exp num)
    (if (number? exp)
	(eq? exp num)
	#f)))

(define make-sum
  (lambda (a1 a2)
    (if (=number? a1 0) a2
	(if (=number? a2 0) a1
	    (if (and (number? a1) (number? a2))
		(+ a1 a2)
		(list '+ a1 a2))))))

(define make-product
  (lambda (m1 m2)
    (if (or (=number? m1 0) (=number? m2 0))
	0
	(if (=number? m1 1)
	    m2
	    (if (=number? m2 1)
		m1
		(if (and (number? m1) (number? m2))
		    (* m1 m2)
		    (list '* m1 m2)))))))

(define sum?
  (lambda (x)
    (if (pair? x)
	(eq? (car x) '+)
	#f)))

(define addend
  (lambda (s)
    (cadr s)))

(define augend
  (lambda (s)
    (caddr s)))

(define product?
  (lambda (x)
    (if (pair? x)
	(eq? (car x) '*))))

(define multiplier
  (lambda (p)
    (cadr p)))

(define multiplicand
  (lambda (p)
    (caddr p)))

(define deriv
  (lambda (exp var)
    (if (number? exp) 0
	(if (variable? exp)
	    (if (same-variable? exp var) 1 0)
	    (if (sum? exp)
		(make-sum (deriv (addend exp) var)
			  (deriv (augend exp) var))
		(if (product? exp)
		    (make-sum
		     (make-product (multiplier exp)
				   (deriv (multiplicand exp) var))
		     (make-product (deriv (multiplier exp) var)
				   (multiplicand exp)))
		    (begin
		      (display "unknown expression type: ")
		      (display exp))))))))

(display (deriv '(+ x 3) 'x))
(newline)
(display (deriv '(* x 3) 'x))
(newline)
(display (deriv '(* x y) 'x))
(newline)
(display (deriv '(* (* x y) (+ x 3)) 'x))
(newline)
