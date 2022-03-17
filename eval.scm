;;; Simple eval/apply interpreter for a small Scheme-like language.
;;; Written for MIT Scheme

(define eval*
  (lambda (exp env)
    (cond ((self-evaluating? exp)       ; 42, "foobar", #f
           exp)
          ((symbol? exp)                ; x, y, z
           (lookup exp env))
          ((eq? (car exp) 'quote)       ; (quote foo)
           (cadr exp))
          ((eq? (car exp) 'define)      ; (define foo 42)
           (define-variable!
             (cadr exp)                 ; variable
             (eval* (caddr exp) env)    ; value
             env))
          ((eq? (car exp) 'if)          ; (if cond exp1 exp2)
           (if (eval* (cadr exp) env)
               (eval* (caddr exp) env)
               (eval* (cadddr exp) env)))
          ((eq? (car exp) 'begin)       ; (begin exp1 exp2 ...)
           (eval-sequence (cdr exp) env))
          ((eq? (car exp) 'lambda)      ; (lambda (param1 param2 ...) body)
           (make-procedure
            (cadr exp)                  ; parameters
            (cddr exp)                  ; body
            env))
          ((macro? exp)                 ; macro
           (eval* (macro-expand exp) env))
          ((pair? exp)                  ; (f exp1 exp2 ... )
           (apply* (eval* (car exp) env)
                   (list-of-values (cdr exp) env)))
          (else
           (error "Unknown expression type" exp)))))

(define apply*
  (lambda (procedure arguments)
    (cond ((eq? (car procedure) 'primitive)
           (apply (cdr procedure) arguments))
          ((eq? (car procedure) 'procedure)
           (eval-sequence
            (caddr procedure)
            (extend-environment
             (cadr procedure)
             arguments
             (cadddr procedure))))
          (else
           (error "Unknown procedure type" procedure)))))

(define eval-sequence
  (lambda (exps env)
    (cond ((null? (cdr exps))
           (eval* (car exps) env))
          (else
           (eval* (car exps) env)
           (eval-sequence (cdr exps) env)))))

(define extend-environment
  (lambda (vars vals env)
    (if (null? vars) env
        (cons
         (cons (car vars) (car vals))
         (extend-environment (cdr vars) (cdr vals) env)))))

(define self-evaluating?
  (lambda (exp)
    (or (number? exp) (string? exp) (boolean? exp))))

(define make-procedure
  (lambda (parameters body env)
    (list 'procedure parameters body env)))

(define list-of-values
  (lambda (exps env)
    (map (lambda (exp) (eval* exp env)) exps)))

(define lookup
  (lambda (var env)
    (let ((v (assoc var env)))
      (if v (cdr v)
          (error "Error: unbound variable" var)))))

(define (prepend! x l)
  (let ((first (car l))
        (rest (cdr l)))
    (set-car! l x)
    (set-cdr! l (cons first rest))))

(define (add-binding! var val env)
  (let ((v (cons var val)))
    (prepend! v env)))

(define (define-variable! var val env)
  (let ((v (assoc var env)))
    (if v
        (set-cdr! v val)
        (add-binding! var val env))))

(define env0                            ; initial environment
  (list (cons 'car (cons 'primitive car))
        (cons 'cdr (cons 'primitive cdr))
        (cons 'cons (cons 'primitive cons))
        (cons 'eq? (cons 'primitive eq?))
        (cons '+ (cons 'primitive +))
        (cons '- (cons 'primitive -))
        (cons '> (cons 'primitive >))
        (cons '< (cons 'primitive <))))

;;; (or e1 e2 ...) => (if e1 #t (or e2 ...))
(define macro-or
  (lambda (operands)
    (if (null? operands) #f
        (list 'if (car operands) #t (macro-or (cdr operands))))))

;;; (and e1 e2 ... ) => (if e1 (and e2 ...) #f)
(define macro-and
  (lambda (operands)
    (if (null? operands) #t
        (list 'if (car operands) (macro-and (cdr operands)) #f))))

;;; (cond ((t1 e1) (t2 e2) ...)) => (if t1 e1 (cond ((t2 e2) ...)))
(define macro-cond
  (lambda (body)
    (if (null? body) '()
        (append
         (list 'if (caar body) (cadar body))
         (list (macro-cond (cdr body)))))))

(define macros                          ; initial macro definitions
  (list (cons 'or macro-or)
        (cons 'and macro-and)
        (cons 'cond macro-cond)))

(define macro?
  (lambda (exp)
    (and
     (pair? exp)
     (assoc (car exp) macros))))

(define macro-expand
  (lambda (macro)
    (let ((m (assoc (car macro) macros)))
      (if m
          ((cdr m) (cdr macro))
          (error "No macro expansion for " macro)))))

(eval* '3 env0)
(eval* 'foo '((foo . 17)))
(eval* '(quote fum) env0)
(eval* '(if 1 (quote a) (quote b)) env0)
(eval* '((lambda (x) x) 42) env0)
(eval* '((lambda (x) (car x)) '(1 2 3)) env0)
(eval* '(begin
          (define i (lambda (x) x))
          (define k (lambda (x) (lambda (y) x)))
          (define s (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))
          (((s k) k) 123)) env0)
(eval* '(or (eq? 3 0) (eq? 1 2) (eq? 5 5)) env0)
(eval* '(and (eq? 3 3) (eq? 2 2)) env0)
(eval* '(and (eq? 3 3) (eq? 2 2) (eq? 5 4)) env0)
(eval* '(begin
          (define zero? (lambda (x) (cond ((eq? x 0) #t) (#t #f))))
          (define abs (lambda (x) (cond ((< x 0) (- 0 x)) ((> x 0) x) (#t 0))))
          (abs -4)) env0)
