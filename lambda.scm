(define id (lambda (x) x))
(id 42)
(define k (lambda (x y) x))
(k 1 2)
(define s (lambda (x y z) (x z (y z))))
(s k k 17)

42
(quote 17)
(quote (1 2 3))
x
(set! x 22)
(define l123 (quote (1 2 3)))
#t
#f
(if #t (quote a) (quote b))
(if #f (quote a) (quote b))
(if 42 (quote a) (quote b))
(car (quote (1 2 3)))
(cdr (quote (1 2 3)))
((if #f + *) 2 3)
(begin (define u 42) (set! u 99) u)

;;; zero? returns #t if x is zero, #f otherwise
(define zero?
  (lambda (n)
    (= n 0)))

(define -1+
  (lambda (n)
    (+ n -1)))

;;; recursive factorial function
;;; this lisp interpreter can compute up to 12!
(define fact
  (lambda (n)
    (if (zero? n)
	1
	(* n (fact (-1+ n))))))

(fact 10)