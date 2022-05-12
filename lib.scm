;;; This file should be loaded initially
;;; to define various utility functions.

(define nil ())

;;; (foldl f (x1 x2 ... xn) base) ==> f(x1, f(x2, (f(..., f(xn, base)))))
(define (foldl f xs base)
  (if (null? xs)
      base
      (f (car xs) (foldl f (cdr xs) base))))

;;; (map f (x1 x2 ...)) ==> (f(x1) f(x2) ...)
(define (map f xs)
  (when (pair? xs)
    (cons (f (car xs)) (map f (cdr xs)))))

(define (newline)
  (display "\n"))

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

;;; (list 1 2 3) ==> (1 2 3)
(define list (lambda l l))

;;; (print x1 x2 ...) -- display all xi
(define print
  (lambda xs
    (map display xs)))

;;; (println x1 x2 ...) -- display all xi, plus a newline
(define println
  (lambda xs
    (map display xs)
    (newline)))

;;; (not x) -- negate x
(define (not x)
  (if x #f #t))

;;; (and x1 x2 ... ) ==> #t if all xs evaluate to #t, otherwise #f
(define and
  (nlambda xs
    (and$ xs $env)))

(define (and$ xs env)
    (if (null? xs)
        #t
        (if (eval (car xs) env)
            (and$ (cdr xs) env)
            #f)))

;;; (or x1 x2 ... ) ==> #f if all xs evaluate to #f, otherwise #t
(define or
  (nlambda xs
    (or$ xs $env)))

(define (or$ xs env)
  (if (null? xs)
      #f
      (if (eval (car xs) env)
          #t
          (or$ (cdr xs) env))))

(define cond
  (nlambda body
    (cond$ body $env)))

(define (cond$ body env)
  (if (null? body)
      #f
      (if (eval (caar body) env)
          (eval (cadar body) env)
          (cond$ (cdr body) env))))

;;; (let ((x 1)
;;;       (y 2))
;;;       (+ x y)) ==> ((lambda (x y) (+ x y)) 1 2)

(define let
  (nlambda body
    (let$ (car body) (cdr body) $env)))

(define (let$ initializers body env)
  (eval
   (cons (cons 'lambda (cons (map car initializers) body)) (map cadr initializers))
   env))

;;; (eval-sequence xs env) -- evaluate all x in xs, in environment env
(define (eval-sequence xs env)
  (cond ((null? xs) nil)
        ((null? (cdr xs)) (eval (car xs) env))
        (#t (begin
              (eval (car xs) env)
              (eval-sequence (cdr xs) env)))))

;;; (when condition x1 x2 ...) ==> (if condition (begin x1 x2 ...))
(define when
  (nlambda body
    (when$ body $env)))

(define (when$ body env)
  (if (eval (car body) env)
      (eval-sequence (cdr body) env)))

;;; (unless x1 x2 ... ) is the opposite of when
(define unless
  (nlambda body
    (unless$ body $env)))

(define (unless$ body env)
  (if (eval (car body) env)
      #f
      (eval-sequence (cdr body) env)))

;;; (apply f x1 x2 ...) ==> apply f to arguments x1 x2 ...
(define apply
  (nlambda body
    (apply$ (car body) (cdr body) $env)))

(define apply$
  (lambda (f args env)
    (eval (cons f (eval-sequence args env)))))


;;; (equal? x y) ==> #t if x and y are structurally equal, #f otherwise
(define (equal? x y)
  (if (and (pair? x) (pair? y))
      (and (equal? (car x) (car y))
           (equal? (cdr x) (cdr y)))
      (eq? x y)))

;;; (assert x y) ==> #t if x == y, otherwise abort
(define (assert x y)
  (unless (equal? x y)
    (print "*** assertion failed, line " (car (file)) " in file " (cadr (file)) "\n")
    (print x " " y "\n")
    (exit)))


(define (>= x y)
    (or (> x y) (eq? x y)))

(define (<= x y)
    (or (< x y) (eq? x y)))

;;; (assoc key alist) ==> return item in alist whose car == key, #f otherwise
(define (assoc key alist)
  (unless (null? alist)
    (if (eq? key (caar alist))
	(car alist)
	(assoc key (cdr alist)))))

(assert (assoc 'c '((a . 1) (b . 2) (c . 3) (d . 4))) '(c . 3))
(assert (assoc 'q '((a . 1) (b . 2) (c . 3) (d . 4))) #f)

;;; append two lists
(define (append x y)
  (if (null? x) y
	    (cons (car x)
	          (append (cdr x) y))))

;;; (length l) ==> length of list
(define (length l)
  (if (null? l)
      0
      (plus 1 (length (cdr l)))))


;;; (+ x1 x2 ... xn) ==> x1 + x2 + ... + xn
(define +
  (lambda xs
    (foldl plus xs 0)))

;;; I'm adapting MIT-Scheme's behaviour here: (-) ==> error
;;; (- 3) ==> -3, (- 10 1) ==> 9, (- 10 1 2) ==> 7
(define -
  (lambda xs
    (cond ((null? xs) (error "- requires at least one argument\n"))
          ((null? (cdr xs)) (minus 0 (car xs)))
          (#t (minus (car xs) (foldl plus (cdr xs) 0))))))


;;; (* x1 x2 ... xn) ==> x1 * x2 * ... xn
(define *
  (lambda xs
    (foldl times xs 1)))

;;; / is pretty much like minus
(define /
  (lambda xs
    (cond ((null? xs) (error "/ requires at least one argument\n"))
          ((null? (cdr xs)) (div 1 (car xs)))
          (#t (div (car xs) (foldl times (cdr xs) 1))))))


;;; (zero? n) ==> #t if n is zero, #f otherwise
(define (zero? n)
    (eq? n 0))

;;; (abs n) ==> absolute value of n
(define (abs n)
  (cond ((< n 0) (- n))
        ((> n 0) n)
        (#t 0)))

;;; (-1+ n) ==> (- n 1)
(define (-1+ n)
  (- n 1))

(define (range x y)
  (when (< x y)
    (cons x (range (+ x 1) y))))

;;; (error x1 x2 ...) -- display all x1, then exit
(define error
  (lambda xs
    (map display xs)
    (exit)))

;;; a mod b = a - b*int(a/b)
(define (mod a b)
  (- a (* b (truncate (/ a b)))))

(define (quotient a b)
  (truncate (/ a b)))

(define pi 3.14159265358979323846264338)

;;; Simple LCG random number generator
;;; X_{n+1} = (a*X_n + c) mod m with m = 2^16+1, a = 75, c = 74
;;; See https://en.wikipedia.org/wiki/Linear_congruential_generator
(define *seed* 0)
(define (random)
  (set! *seed* (mod (+ (* 75 *seed*) 74) 65537))
  *seed*)

;;; Some basic sanity checking of built-in and defined functions

(assert (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

(assert (car (list 1 2)) 1)
(assert (car (cdr (list 1 2))) 2)
(assert (car (list 'a 'b)) 'a)

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
(assert (minus 0 1) -1)

(assert (length '(1 2 3)) 3)
(assert (- 3) -3)
(assert (- -3) 3)
(assert (- 10 1) 9)
(assert (- 10 1 2) 7)

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

(assert
 (let ((x 1)
       (y 2))
   (plus x y)) 3)

(assert (zero? 0) #t)
(assert (zero? 17) #f)

(assert (abs -3) 3)
(assert (abs 3) 3)
(assert (abs 0) 0)

(assert (-1+ 3) 2)

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
(assert (range 1 5) '(1 2 3 4))
(assert (map abs '(-3 1 -4)) '(3 1 4))
(assert (mod 10 3) 1)
(assert (quotient 10 3) 3)

(display "testing...") (newline)

(assert (map zero? '(0 1 2)) '(#t #f #f))

(assert (apply + '(1 2)) 3)

(assert (random) 74)

(display "lib.scm loaded\n")
