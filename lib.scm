(define assert
  (lambda (x y)
    (if (eq? x y)
	#t
	(begin
	  (display "*** assert failed\n")
	  (exit)))))
 
(define list (lambda l l))

(define cadr
  (lambda (l)
    (car (cdr l))))

(define cddr
  (lambda (l)
    (cdr (cdr l))))

(define binary-and
  (lambda (x y)
    (if x y #f)))

(define binary-or
  (lambda (x y)
    (if x #t y)))

(define foldl
  (lambda (f l base)
    (if (null? l)
	base
	(f (car l) (foldl f (cdr l) base)))))	

(define +
  (lambda l
    (foldl plus l 0)))

(define -
  (lambda l
    (foldl minus l 0)))

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

