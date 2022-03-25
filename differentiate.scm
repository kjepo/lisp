;;; Symbolic differentiation, adapted from chapter 2.3.2
;;; "Structure and Interpretation of Computer Programs".

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (if (and (variable? v1) (variable? v2))
	    (eq? v1 v2)
	    #f))

(define (=number? exp num)
  (if (number? exp)
	    (eq? exp num)
	    #f))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (#t (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (#t (list '* m1 m2))))

(define (sum? x)
  (if (pair? x)
	    (eq? (car x) '+)
	    #f))

(define (addend s)
  (cadr s))

(define (augend s)
  (caddr s))

(define (product? x)
  (if (pair? x)
	    (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
		     (make-sum
		      (make-product (multiplier exp)
				                (deriv (multiplicand exp) var))
		      (make-product (deriv (multiplier exp) var)
				                (multiplicand exp))))
        (#t (error "unknown expression type: " exp))))

(print "(+ x 3) => " (deriv '(+ x 3) 'x) "\n")
(print "(* x 3) => " (deriv '(* x 3) 'x) "\n")
(print "(* x y) => "(deriv '(* x y) 'x) "\n")
(print "(* (* x y) (+ x 3)) => " (deriv '(* (* x y) (+ x 3)) 'x) "\n")
