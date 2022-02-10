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

(list 1 2 3)
(list (* 3 3) (* 4 4))
(list 'a 1 '(3 1 4))

'a
(car '(1 2 3))
(define empty-list '())
empty-list
(cons 1 empty-list)

(number? 1)
(number? 'a)
(number? (list 1 2 3))
(symbol? 1)
(symbol? 'a)
(symbol? (list 1 2 3))

(define sign
  (lambda (n)
    (if (< n 0) -1 1)))

(sign 17)
(sign -42)

(display "Hello, world\nThis is a tab\tcharacter")
(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6)) ; ==> 57

(define square
  (lambda (x)
    (* x x))

