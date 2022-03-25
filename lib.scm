;;; This file should be loaded initially
;;; to define various utility functions.

(define nil ())

(define newline
  (lambda ()
    (display "\n")))

;;; (not x) -- negate x
(define not
  (lambda (x)
    (if x #f #t)))

;;; (and x1 x2 ... ) ==> #t if all xs evaluate to #t, otherwise #f
(define and
  (nlambda xs
           (and$ xs $env)))

(define and$
  (lambda (xs env)
    (if (null? xs)
        #t
        (if (eval (car xs) env)
            (and$ (cdr xs) env)
            #f))))

;;; (or x1 x2 ... ) ==> #f if all xs evaluate to #f, otherwise #t
(define or
  (nlambda xs
           (or$ xs $env)))

(define or$
  (lambda (xs env)
    (if (null? xs)
        #f
        (if (eval (car xs) env)
            #t
            (or$ (cdr xs) env)))))

(define cond
  (nlambda body
    (cond$ body $env)))

(define cond$
  (lambda (body env)
    (if (null? body)
        #f
        (if (eval (caar body) env)
            (eval (cadar body) env)
            (cond$ (cdr body) env)))))

;;; (let ((v1 e1) (v2 e2) ...))


;;; (defun (foo x1 x2 ...) body) ==> (define foo (lambda (x1 x2 ...) body))

(define defun
  (nlambda body
    (defun* body $env)))

(define defun*
  (lambda (body env)
    (display (caar body))
    (define (caar body)
      (eval (list 'lambda (cdar body) (cons 'begin (cdr body)))))))

;
;    (eval
;     (list 'define
;           (caar body)
;           (list 'lambda (cdar body) (cons 'begin (cdr body))))
;     env)))

;;; (eval-sequence xs env) -- evaluate all x in xs, in environment env
(define eval-sequence
  (lambda (xs env)
    (cond ((null? xs) nil)
          ((null? (cdr xs)) (eval (car xs) env))
          (#t (begin
                (eval (car xs) env)
                (eval-sequence (cdr xs) env))))))

;;; (when condition x1 x2 ...) ==> (if condition (begin x1 x2 ...))
(define when
  (nlambda body
    (when$ body $env)))

(define when$
  (lambda (body env)
    (if (eval (car body) env)
        (eval-sequence (cdr body) env))))

;;; (equal? x y) ==> #t if x and y are structurally equal, #f otherwise
(define equal?
  (lambda (x y)
    (if (and (pair? x) (pair? y))
        (and (equal? (car x) (car y))
             (equal? (cdr x) (cdr y)))
        (eq? x y))))

;;; (assert x y) => #t if x == y, otherwise abort
(define assert
  (lambda (x y)
    (if (equal? x y)
        #t
        (begin
          (display "*** assertion failed, line ")
          (display (car (file)))
          (display " in file ")
          (display (car (cdr (file))))
          (newline)
	        (display x) (display " ") (display y) (newline)
	        (display (cadr x)) (display " ") (display (cadr y)) (newline)
	        (display (caddr x)) (display " ") (display (caddr y)) (newline)
	        (display (equal? x y)) (newline)
	        (display (car x)) (display " ") (display (car y)) (newline)
	        (display (eq? (car x) (car y))) (newline)
	        (newline)
          (exit)))))

(assert (and) #t)
(assert (and #t) #t)
(assert (and #f) #f)
(assert (and #t #t) #t)
(assert (and #f #t) #f)
(assert (and #t #f) #f)
(assert (and #f #f) #f)
(assert (and #f (display "oops, this shouldn't happen\n")) #f)

(assert (or) #f)
(assert (or #t) #t)
(assert (or #f) #f)
(assert (or #t #t) #t)
(assert (or #f #t) #t)
(assert (or #t #f) #t)
(assert (or #f #f) #f)

(define >=
  (lambda (x y)
    (if (> x y) #t (eq? x y))))

(define <=
  (lambda (x y)
    (if (< x y) #t (eq? x y))))

;;; some basic sanity checking of built-in functions
(assert (< 2 3) #t)
(assert (<= 2 3) #t)
(assert (<= 3 3) #t)
(assert (< 3 3) #f)
(assert (> 3 2) #t)
(assert (>= 3 2) #t)
(assert (>= 3 3) #t)
(assert (> 3 3) #f)

(assert (car (cons 1 2)) 1)
(assert (cdr (cons 1 2)) 2)
(assert (number? 1) #t)
(assert (number? 'a) #f)
(assert (number? '(1 2 3)) #f)
(assert (symbol? 1) #f)
(assert (symbol? 'a) #t)
(assert (symbol? '(1 2 3)) #f)
(assert (plus (times 3 (plus (times 2 4) (plus 3 5))) (plus (minus 10 7) 6)) 57)

;;; (assert (minus 0 1) -1)
 
;;; append two lists
(define append
  (lambda (x y)
    (if (null? x) y
	      (cons (car x)
	            (append (cdr x) y)))))

(assert (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

;;; (list 1 2 3) => (1 2 3)
(define list (lambda l l))

(assert (car (list 1 2)) 1)
(assert (car (cdr (list 1 2))) 2)
(assert (car (list 'a 'b)) 'a)

;;; (length l) => length of list
(define length
  (lambda (l)
    (if (null? l)
        0
        (plus 1 (length (cdr l))))))

(assert (length '(1 2 3)) 3)

;;; not all 28 combinations, but a few...
(define caar  (lambda (l) (car (car l))))
(define cadr  (lambda (l) (car (cdr l))))
(define cdar  (lambda (l) (cdr (car l))))
(define cddr  (lambda (l) (cdr (cdr l))))
(define cdddr (lambda (l) (cdr (cdr (cdr l)))))
(define caddr (lambda (l) (car (cdr (cdr l)))))
(define caadr (lambda (l) (car (car (cdr l)))))
(define cadar (lambda (l) (car (cdr (car l)))))
(define first (lambda (l) (car l)))
(define rest  (lambda (l) (cdr l)))

;;; (foldl f (t_1 t_2 ... t_n) base) => f(t_1, f(t_2, (f(..., f(t_n, base)))))
(define foldl
  (lambda (f xs base)
    (if (null? xs)
        base
        (f (car xs) (foldl f (cdr xs) base)))))   

;;; (+ x1 x2 ... xn) => x1 + x2 + ... + xn
(define +
  (lambda xs
    (foldl plus xs 0)))

;;; I'm adapting MIT-Scheme's behaviour here: (-) => error
;;; (- 3) => -3, (- 10 1) => 9, (- 10 1 2) => 7
(define -
  (lambda xs
    (cond ((null? xs) (error "- requires at least one argument\n"))
          ((null? (cdr xs)) (minus 0 (car xs)))
          (#t (minus (car xs) (foldl plus (cdr xs) 0))))))

(assert (- 3) -3)
(assert (- -3) 3)
(assert (- 10 1) 9)
(assert (- 10 1 2) 7)

;;; (* x1 x2 ... xn) => x1 * x2 * ... xn
(define *
  (lambda xs
    (foldl times xs 1)))

;;; / is pretty much like minus
(define /
  (lambda xs
    (cond ((null? xs) (error "/ requires at least one argument\n"))
          ((null? (cdr xs)) (div 1 (car xs)))
          (#t (div (car xs) (foldl times (cdr xs) 1))))))

(assert (+ 1 2 3 4 5 6 7 8 9 10) 55)
(assert (+ 3) 3)
(assert (+) 0)
(assert (* 1 2 3) 6)
(assert (*) 1)
(assert (* 3) 3)
(assert (- 10 1 2) 7)

(assert (and) #t)
(assert (and #f) #f)
(assert (and #t) #t)
(assert (and #f (display "error in and: this should not be seen\n")) #f)
(assert (and #t #t #t) #t)
(assert (and #t #t #f) #f)
(assert (and #t #f #t) #f)
(assert (and #f #t #t) #f)
(assert (and #f #f #f) #f)
(assert (or) #f)
(assert (or #f) #f)
(assert (or #t) #t)
(assert (or #t (display "error in or: this should not be seen\n")) #t)
(assert (or #t #t #t) #t)
(assert (or #t #t #f) #t)
(assert (or #t #f #t) #t)
(assert (or #f #t #t) #t)
(assert (or #f #f #f) #f)

;;; (zero? n) ==> #t if n is zero, #f otherwise
(define zero?
  (lambda (n)
    (eq? n 0)))

(assert (zero? 0) #t)
(assert (zero? 17) #f)

;;; (abs n) ==> absolute value of n
(define abs
  (lambda (n)
    (cond ((< n 0) (- n))
          ((> n 0) n)
          (#t 0))))

(assert (abs -3) 3)
(assert (abs 3) 3)
(assert (abs 0) 0)

;;; (-1+ n) ==> (- n 1)
(define -1+
  (lambda (n)
    (- n 1)))

(assert (-1+ 3) 2)

(define abs
  (lambda (n)
    (if (< n 0)
        (- n)
        n)))

(assert (equal? 'a 'a) #t)
(assert (equal? 'a 'b) #f)
(assert (equal? 3 3) #t)
(assert (equal? 3 5) #f)
(assert (equal? '(1 2) '(1 2)) #t)
(assert (equal? '(1 2) '(2 1)) #f)
(assert (equal? '(1 2) '(1 (2))) #f)
(assert (equal? '(1 2) '(1)) #f)
(assert (equal? '(1) '(1 2)) #f)
(assert (equal? '(1 (2 3)) '(1 (2 3))) #t)
(assert (equal? '((1 2) 3) '(1 (2 3))) #f)

(define range
  (lambda (x y)
    (when (< x y)
      (cons x (range (+ x 1) y)))))

(assert (range 1 5) '(1 2 3 4))

(define map
  (lambda (f l)
    (if (null? l)
        '()
        (cons (f (car l)) (map f (cdr l))))))

(assert (map abs '(-3 1 -4)) '(3 1 4))
(assert (map zero? '(0 1 2)) '(#t #f #f))

;;; (print x1 x2 ...) -- display all xi
(define print
  (lambda xs
    (map display xs)))

;;; (error x1 x2 ...) -- display all x1, then exit
(define error
  (lambda xs
    (map display xs)
    (exit)))

;;; a mod b = a - b*int(a/b)
(define mod
  (lambda (a b)
    (- a (* b (/ a b)))))

;;; Simple LCG random number generator
;;; X_{n+1} = (a*X_n + c) mod m with m = 2^16+1, a = 75, c = 74
;;; See https://en.wikipedia.org/wiki/Linear_congruential_generator
(define *seed* 0)
(define random
  (lambda ()
    (set! *seed* (mod (+ (* 75 *seed*) 74) 65537))
    *seed*))

(display "lib.scm loaded\n")
