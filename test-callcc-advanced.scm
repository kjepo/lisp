;; Test 1: Basic escape
(display "Test 1: ")
(display (call/cc (lambda (k) (+ 5 (k 10)))))
(newline)

;; Test 2: Normal return (don't invoke k)
(display "Test 2: ")
(display (call/cc (lambda (k) (+ 5 10))))
(newline)

;; Test 3: Nested computation with escape
(display "Test 3: ")
(display (+ 1 (call/cc (lambda (k) (* 2 (k 3))))))
(newline)

;; Test 4: Storing and invoking continuation multiple times
(define saved-cont #f)
(display "Test 4a: ")
(display (+ 100 (call/cc (lambda (k) (set! saved-cont k) 5))))
(newline)

(display "Test 4b: ")
(display (saved-cont 20))
(newline)

(exit)
