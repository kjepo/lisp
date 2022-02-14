(define newline
  (lambda ()
    (display "\n")))

;;; binary-and and binary-or should not evaluate both of its arguments
;;; but since we don't have macros yet, this will have to suffice.

;;; (binary-and x y) => x && y
(define binary-and
  (lambda (x y)
    (if x y #f)))

;;; (binary-or x y) => x || y
(define binary-or
  (lambda (x y)
    (if x #t y)))

;;; (equal? x y) ==> #t if x and y are structurally equal, #f otherwise
(define equal?
  (lambda (x y)
    (if (binary-and (pair? x) (pair? y))
        (binary-and (equal? (car x) (car y))
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
          (exit)))))

;;; some basic sanity checking of built-in functions
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
(define cddr  (lambda (l) (cdr (cdr l))))
(define cdddr (lambda (l) (cdr (cdr (cdr l)))))
(define caddr (lambda (l) (car (cdr (cdr l)))))
(define first (lambda (l) (car l)))
(define rest  (lambda (l) (cdr l)))

;;; (foldl f (t_1 t_2 ... t_n) base) => f(t_1, f(t_2, (f(..., f(t_n, base)))))
(define foldl
  (lambda (f l base)
    (if (null? l)
        base
        (f (car l) (foldl f (cdr l) base)))))   

;;; (+ t_1 t_2 ... t_n) => t_1 + t_2 + ... t_n
(define +
  (lambda l
    (foldl plus l 0)))

;;; I'm adapting MIT-Scheme's behaviour here: (-) => error, (- 3) => -3, (- 10 1) => 9, (- 10 1 2) => 7

(define -
  (lambda l
    (if (eq? (length l) 0)
        (begin
          (display "error: - requires at least one argument\n")
          (exit))
        (if (eq? (length l) 1)
            (minus 0 (car l))
            (minus (car l) (foldl plus (cdr l) 0))))))

(assert (- 3) -3)
(assert (- -3) 3)
(assert (- 10 1) 9)
(assert (- 10 1 2) 7)

;;; (* t_1 t_2 ... t_n) => t_1 * t_2 * ... t_n
(define *
  (lambda l
    (foldl times l 1)))

(define and
  (lambda l
    (foldl binary-and l #t)))

(define or
  (lambda l
    (foldl binary-or l #f)))

(assert (+ 1 2 3 4 5 6 7 8 9 10) 55)
(assert (+ 3) 3)
(assert (+) 0)
(assert (* 1 2 3) 6)
(assert (*) 1)
(assert (* 3) 3)
; (minus 10 1 2)  ; fixme doesn't work

(assert (and #t #t #t) #t)
(assert (and #t #t #f) #f)
(assert (and #t #f #t) #f)
(assert (and #f #t #t) #f)
(assert (and #f #f #f) #f)
(assert (or #t #t #t) #t)
(assert (or #t #t #f) #t)
(assert (or #t #f #t) #t)
(assert (or #f #t #t) #t)
(assert (or #f #f #f) #f)

;;; (zero? n) => #t if n is zero, #f otherwise
(define zero?
  (lambda (n)
    (eq? n 0)))

(assert (zero? 0) #t)
(assert (zero? 17) #f)

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

(define map
  (lambda (f l)
    (if (null? l)
        '()
        (cons (f (car l)) (map f (cdr l))))))

(assert (map abs '(-3 1 -4)) '(3 1 4))

