# explicit LISP

This is an exercise in building a LISP machine.

The code in `explicit.scm` is a register machine implementation
of a LISP interpreter. Rather than the usual recursive apply/eval
description of the interpreter, we have made the control and
arguments explicit.  The code is based on SICP, chapter 5 (and forwards).

The goal is now to write the same code in C (file `lisp.c`).
The code is not finished yet but a rudimentary interpreter is working
and can handle the examples below.
```
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
; (list 'a 1 '(3 1 4))

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
```
The informal description of the language is as follows:
```
expr    : symbol | ( expr* ) | ' expr
symbol  : ID | NUMBER | STRING
```

where `ID` is an identifier (letters optionally followed by letters and digits, and
where letters also include the special characters `#+-.*/<=>!?:$%_&~^`,
`NUMBER` is an optional sign (`+` or `-`) followed by one or more digits,
and `STRING` is a `"`-enclosed string which can contain the usual escape
sequences `\n`, `t`, etc.

There are some built-in constants and primitives:

- `#f` denotes false.
- `#t` denotes true.
- `quote`, e.g., `(quote x)` yields `x`, `(quote (1 2 3))` yields `(1 2 3)`.
The shorthand `'x` can be used instead of `(quote x)`.
- `if`, e.g., `(if x y z)` evaluates `y` if `x` evaluates to anything but `#f` and `z` if `x`
evaluates to `#f`.  If `z` is omitted, the result is unspecified.
- `(define x y)` evaluates `y` and binds it to `x`.  If there was a previous binding for `x`, 
it is overwritten.

# Future plans

While I'm not aiming to write a fully fledged Scheme interpreter there
are a few things I'd like to do:

- Add garbage collection
- Change the boxed representation to use lsb for the tag.
- Catch C-c and use GNU's readline library to parse input from stdin.
- Add more primitives, for instance `let`, `cond`
- Add strings

# References

Abelson and Sussman *Structure and Interpretation of Computer Programs*:
<br>
https://mitpress.mit.edu/sites/default/files/sicp/index.html



