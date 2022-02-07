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

(id 99)
(define x 17)
x
(id 98)
x
(set! x 0)
x

