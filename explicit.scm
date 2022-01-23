; Explicit evaluator for small LISP written in Scheme /kjepo 2021-01-23

; Emacs: start Scheme with M-x run-scheme
; - hit C-x C-e at the end of an expression to evaluate it
; - hit C-c C-r to evaluate the region

;;; Debugging
(define debug #f)
(define (dbg msg)
  (if debug
      (begin (display "\n -->") (display msg) (newline))))

;;; Stack operations: save and restore
(define stack '())

(define (save x)
  (set! stack (append (list x) stack)))

(define (pop)
  (if (null? stack)
      (error "Error: pop on empty stack"))
  (define x (car stack))   ;; fixme: change to let?
  (set! stack (cdr stack))
  x)

(define-syntax restore
  ; So we can say (restore val) instead of (set! val (pop))
  (syntax-rules ()
    ((restore var)
     (set! var (pop)))))

;;; Shorthand to make tail-recursive call explicit
(define (goto p)
  (p))

;;; Primitive procedures

(define (is-primitive-procedure? proc)
  (procedure? proc))

(define (is-compound-procedure? proc)
  (tagged-list? proc 'procedure))

(define (apply-primitive-procedure proc args)
  (apply proc args))

(define primitive-procedures
  (list
   (list 'car car)
   (list 'cdr cdr)
   (list 'cons cons)
   (list 'null? null?)))

;;; Helper functions

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define (is-begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp)
  (cdr exp))

(define (last-exp? seq)
  (null? (cdr seq)))

(define (first-exp seq)
  (car seq))

(define (rest-exps seq)
  (cdr seq))

(define (is-application? exp)
  (pair? exp))

(define (operator exp)
  (car exp))
 
(define (operands exp)
  (cdr exp))

(define (empty-arglist)
  '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (no-operands? ops)
  (null? ops))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))

(define (last-operand? ops)
  (null? (cdr ops)))

(define (procedure-parameters p)
  (cadr p))

(define (procedure-body p)
  (caddr p))

(define (is-self-evaluating? exp)
  (or (number? exp) (string? exp)))

(define (is-variable? exp)
  (symbol? exp))

(define (is-quote? exp)
  (eq? (car exp) 'quote))

(define (is-if? exp)
  (eq? (car exp) 'if))

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (is-assignment? exp)
  (eq? (car exp) 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

(define (is-definition? exp)
  (eq? (car exp) 'define))

(define (definition-variable exp)
  (cadr exp))

(define (definition-value exp)
  (caddr exp))

(define (is-lambda? exp)
  (eq? (car exp) 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (text-of-quotation exp)
  (cadr exp))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

;;; Environment

(define (procedure-environment p)
  (cadddr p))

(define (lookup var env)
  (let ((v (assoc var env)))
    (if v (cadr v)
	(error "Error: unbound variable" var))))

(define (pair-up vars vals)
  ; we assume vars and vals have same length
  (if (null? vars) '()
      (cons (list (car vars) (car vals))
	    (pair-up (cdr vars) (cdr vals)))))

(define (bind vars vals env)
  (append (pair-up vars vals) env))

(define (set-variable-value! var val env)
  (let ((v (assoc var env)))
    (if v (set-car! (cdr v) val)
	(error "Error: unbound variable" var))))

(define (define-variable! var val env)
  (let ((v (assoc var env)))
    (if v (set-car! (cdr v) val)
	(add-binding! var val env))))

(define (add-binding! var val env)
  (let ((v (list var val)))
    (prepend! v env)))

(define (prepend! x l)
  (let ((first (car l))
	(rest (cdr l)))
    (set-car! l x)
    (set-cdr! l (cons first rest))))

(define (explicit-eval expression retfcn)

  (define (display-registers)
    (if debug
	(begin
	  (newline)
	  (display "exp: ")   (display exp) (newline)
	  (display "env: ")   (display env) (newline)
	  (display "cont: ")  (display cont) (newline)
	  (display "val: ")   (display val) (newline)
	  (display "unev: ")  (display unev) (newline)
	  (display "argl: ")  (display argl) (newline)
	  (display "proc: ")  (display proc) (newline)
	  (display "stack: ") (display stack) (newline))))

  ;;; Number/Atom

  (define (ev-self-eval)
    (dbg 'ev-self-eval)
    (set! val exp)
    (goto cont))

  ;;; Variable

  (define (ev-variable)
    (dbg 'ev-variable)
    (set! val (lookup exp env))
    (goto cont))

  ;;; Quote

  (define (ev-quoted)
    (dbg 'ev-quoted)
    (set! val (text-of-quotation exp))
    (goto cont))

  ;;; Lambda

  (define (ev-lambda)
    (dbg 'ev-lambda)
    (set! unev (lambda-parameters exp))
    (set! exp (lambda-body exp))
    (set! val (make-procedure unev exp env))
    (goto cont))
  
  ;;; If
  
  (define (ev-if)
    (dbg 'ev-if)
    (save exp) ; save expression for later
    (save env)
    (save cont)
    (set! cont ev-if-decide)
    (set! exp (if-predicate exp))
    (goto eval-dispatch))

  (define (ev-if-decide)
    (dbg 'ev-if-decide)
    (restore cont)
    (restore env)
    (restore exp)
    (if val
	(goto ev-if-consequent)
	(goto ev-if-alternative)))

  (define (ev-if-consequent)
    (dbg 'ev-if-consequent)
    (set! exp (if-consequent exp))
    (goto eval-dispatch))

  (define (ev-if-alternative)
    (dbg 'ev-if-alternative)
    (set! exp (if-alternative exp))
    (goto eval-dispatch))

  ;;; Assignment

  (define (ev-assignment)
    (dbg 'ev-assignment)
    (set! unev (assignment-variable exp))
    (save unev)
    (set! exp (assignment-value exp))
    (save env)
    (save cont)
    (set! cont ev-assignment-1)
    (goto eval-dispatch))

  (define (ev-assignment-1)
    (dbg 'ev-assignment-1)
    (restore cont)
    (restore env)
    (restore unev)
    (set-variable-value! unev val env)
    (set! val 'OK)
    (goto cont))

  ;;; Definition

  (define (ev-definition)
    (dbg 'ev-definition)
    (set! unev (definition-variable exp))
    (save unev)
    (set! exp (definition-value exp))
    (save env)
    (save cont)
    (set! cont ev-definition-1)
    (goto eval-dispatch))

  (define (ev-definition-1)
    (dbg 'ev-definition-1)
    (restore cont)
    (restore env)
    (restore unev)
    (define-variable! unev val env)
    (set! val 'OK)
    (goto cont))

  ;;; Application

  (define (ev-application)
    (dbg 'ev-application)
    (save cont)
    (save env)
    (set! unev (operands exp))
    (save unev)
    (set! exp (operator exp)) 
    (set! cont ev-appl-did-operator)
    (goto eval-dispatch))

  (define (ev-appl-did-operator)
    (dbg 'ev-appl-did-operator)    
    (restore unev)
    (restore env)
    (set! argl (empty-arglist))   ;
    (set! proc val)
    (if (no-operands? unev)
	(goto apply-dispatch))
    (save proc)
    (goto ev-appl-operand-loop))

  (define (ev-appl-operand-loop)
    (dbg 'ev-appl-operand-loop)    
    (save argl)
    (set! exp (first-operand unev))
    (if (last-operand? unev)
	(goto ev-appl-last-arg))
    (save env)
    (save unev)
    (set! cont ev-appl-accumulate-arg)
    (goto eval-dispatch))

  (define (ev-appl-accumulate-arg)
    (dbg 'ev-appl-accumulate-arg)    
    (restore unev)
    (restore env)
    (restore argl)
    (set! argl (adjoin-arg val argl))
    (set! unev (rest-operands unev))
    (goto ev-appl-operand-loop))
  
  (define (ev-appl-last-arg)
    (dbg 'ev-appl-last-arg)    
    (set! cont ev-appl-accum-last-arg)
    (goto eval-dispatch))

  (define (ev-appl-accum-last-arg)
    (dbg 'ev-appl-accum-last-arg)    
    (restore argl)
    (set! argl (adjoin-arg val argl))
    (restore proc)
    (goto apply-dispatch))

  (define (apply-dispatch)
    (dbg 'apply-dispatch)
    (if (is-primitive-procedure? proc)
	(goto primitive-apply))
    (if (is-compound-procedure? proc)
	(goto compound-apply))
    (goto unknown-procedure-type))

  (define (primitive-apply)
    (dbg 'primitive-apply)
    (set! val (apply-primitive-procedure proc argl))
    (restore cont)
    (goto cont))

  (define (compound-apply)
    (dbg 'compound-apply)
    (set! unev (procedure-parameters proc))
    (set! env (procedure-environment proc))
    (set! env (bind unev argl env))
    (set! unev (procedure-body proc))
    (goto ev-sequence))

  ;;; Begin

  (define (ev-begin)
    (dbg 'ev-begin)
    (set! unev (begin-actions exp))
    (save cont)
    (goto ev-sequence))
  
  (define (ev-sequence)
    (dbg 'ev-sequence)
    (set! exp (first-exp unev))
    (if (last-exp? unev)
	(goto ev-sequence-last-exp))
    (save unev)
    (save env)
    (set! cont ev-sequence-continue)
    (goto eval-dispatch))

  (define (ev-sequence-continue)
    (dbg 'ev-sequence-continue)
    (restore env)
    (restore unev)
    (set! unev (rest-exps unev))
    (goto ev-sequence))

  (define (ev-sequence-last-exp)
    (dbg 'ev-sequence-last-exp)
    (restore cont)
    (goto eval-dispatch))

  ;;; Exit points

  (define (print-result)
    (display "\n===> ")
    (display val)
    (retfcn val))

  (define (unknown-procedure-type)
    (display "Error: unknown procedure")
    (display proc)
    (set! val #f)
    (goto cont))

  (define (unknown-expression-type)
    (display "Error: unknown expression")
    (set! val #f)
    (goto cont))

  (define (eval-dispatch)
    (dbg 'eval-dispatch)
    (display-registers)
    (cond ((is-self-evaluating? exp) (goto ev-self-eval))
	  ((is-variable? exp) (goto ev-variable))
	  ((is-quote? exp) (goto ev-quoted))
	  ((is-if? exp) (goto ev-if))
	  ((is-assignment? exp) (goto ev-assignment))
	  ((is-definition? exp) (goto ev-definition))
	  ((is-lambda? exp) (goto ev-lambda))
	  ((is-begin? exp) (goto ev-begin))
	  ((is-application? exp) (goto ev-application))
	  (else (goto unknown-expression-type))))
  
  (set! stack '())
  (define exp expression)
  (define env primitive-procedures)
  (define cont print-result)
  (define val #f)
  (define unev #f)
  (define argl #f)
  (define proc #f)

  (goto eval-dispatch))

(define (eval-and-print expression)
  (call-with-current-continuation
   (lambda (k)
     (explicit-eval expression k))))

(eval-and-print '(if 1 (quote a) (quote b)))
(eval-and-print '(quote 42))
(eval-and-print '(quote (1 2 3)))
(eval-and-print '(lambda (x y) x))
(eval-and-print '((lambda (x) x) 3))
(eval-and-print '(car (quote (1 2 3))))
(eval-and-print '(begin (car (quote (17 99 11)))))
(eval-and-print '(begin (define x 42) (set! x 99) x))
(eval-and-print '((lambda (x) (if (null? x) (quote EMPTY) (quote PAIR))) (quote (1))))
(eval-and-print '((lambda (x y) (car x)) (quote (17 42 69)) (quote (4 5 6))))
(eval-and-print '(begin (define x '(42 17 99)) (car x)))
(eval-and-print
 '(begin
    (define i (lambda (x) x))
    (define k (lambda (x) (lambda (y) x)))
    (define s (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
    (((s k) k) 99)))
(eval-and-print '(quote 9))

