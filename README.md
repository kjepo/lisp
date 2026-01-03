# explicit Lisp

<img src="assets/lisp-machine-logo.png" alt="LISP Machine logo" width="300">

News flash: I have rewritten the Lisp machine to use so called NaN boxing
for storing S-expressions, symbols, numbers, etc.  This will be described
later, but you can now use double precision floating point as well. 

This is an exercise in building a Lisp machine in a series of steps
so that (ultimately) we _could_ write it in assembler.
Why? Yes, why do some people go out in the wilderness and survive
only with the help of a knife when they could sit comfortably at home,
eat junk food and watch TV?

Anyway...

Any college course in Lisp or functional programming is likely
to cover the implementation of a Lisp interpreter in Lisp itself.
(If you are not familiar with the traditional eval/apply rendition of a
Lisp interpreter, please watch "The most beautiful program ever written"
by William Byrd, [https://www.youtube.com/watch?v=OyfBQmvr2Hc]) or read
"Structure and Interpretation of Computer Programs" (SICP) chapter 4.
I have provided a sample implementation `eval.scm` which should
run in, e.g., MIT Scheme.

The interpreter in `eval.scm` takes advantage of the host language
for garbage collection, recursion, etc.  The code in `explicit.scm` 
is a register machine implementation of the interpreter, still written 
in Scheme.  Rather than the recursive apply/eval description we have 
made the control and arguments explicit so that we don't depend on 
the host language for recursion.
The code is based on SICP, chapter 5 (and forwards).

The next step was to write the same code in C (file `lisp.c`) to
get even closer to the machine, represent objects and implement 
garbage collection.  The interpreter is now working and can load 
the library file `lib.scm`.  Here is a small example:

```
;;; zero? returns #t if x is zero, #f otherwise
(define zero?
  (lambda (n)
    (eq? n 0)))

(define -1+
  (lambda (n)
    (+ n -1)))

;;; recursive factorial function
;;; this lisp interpreter can store integers up to 11!
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (-1+ n))))))

(display "11! = ")
(display (fact 11))
(newline)
```

A larger example can be found in the file `differentiate.scm`: just type
`./lisp differentiate.scm` to run it.

Currently, the interpreter can read and parse Lisp - both from standard input
(using GNU's readline), or from external files. The register machine can evaluate
Lisp expressions with proper tail recursion and invoke a stop-and-copy garbage
collector when it runs out of memory.  The parser and interpreter is only about 
960 lines of code, but some external files handle printing, the symbol table and 
garbage collection.

The informal description of the language is as follows:
```
expr    : symbol | ( expr* ) | ( expr+ . expr ) | ' expr
symbol  : ID | NUMBER | STRING
```

where `ID` is an identifier (letters optionally followed by letters and digits, and
where letters also include the special characters `#+-.*/<=>!?:$%_&~^`,
`NUMBER` is an optional sign (`+` or `-`) followed by one or more digits and an optional
decimal part, and finally `STRING` is a `"`-enclosed string which can contain the usual escape
sequences `\n`, `t`, etc. Comments begin with `;` and run to the end of the line.

There are some built-in constants and primitives:

- `#f` denotes false.
- `#t` denotes true.
- `(quote x)` yields `x`, `(quote (1 2 3))` yields `(1 2 3)`.
The shorthand `'x` can be used instead of `(quote x)`.  For instance, `'()` is the empty list.  
- `(if x y z)` evaluates `y` if `x` evaluates to anything but `#f` and `z` if `x`
evaluates to `#f`.  If `x` is false and `z` is omitted, the result is unspecified.
- `(define x y)` evaluates `y` and binds it to `x`.  If there was a previous binding for `x`, 
it is overwritten.
- `(set! x y)` sets `x` to the value of `y`.  If `x` has not previously been defined, an error
is generated.
- `(begin expr₁ expr₂ ... )` evaluates `expr₁`, `expr₂`, etc from left to right.
- `(lambda (p₁ p₂ ... ) body)` evaluates to a procedure which later can be applied to arguments, e.g.,

```
((lambda (x y) (+ (* x x) (* y y))) 3 4) ⇒ 25
```

because `x` and `y` is bound to `3` and `4`, respectively.
The parameter list is optional, i.e.,

```
(define foo (lambda () (display 'foo!)))
```

defines a function `foo` which when invoked with `(foo)` outputs `foo!`.
Also, there is a mechanism to capture a variable number of arguments: with

```
(define list (lambda l l)
```
all the arguments are bound to the formal parameter `l` so that
`(list 1 2 3) ⇒ (1 2 3)`. Finally, the `body` of a `lambda` expression can be a list
of expressions, for instance

```
(define *seed* 0)
(define next
  (lambda ()
    (set! *seed* (+ *seed* 1))
    *seed*))
```
Each invocation of `(next)` yields another integer `1`, `2`, `3`, etc.

- `(procedure arg₁ arg₂  ...)` applies `procedure` to the arguments `arg₁`, `arg₂`, etc,
where `procedure` is either the result of a `lambda` expression, or one of the built-in
functions `car`, `cdr`, `cons`, `pair?`, `+`, `-`, `*`, `<`, `>`, `display`, `list`,
`number?`, `symbol?`, `eq?`, `file`, and `call/cc` (also `call-with-current-continuation`).

# Primitives

- The primitive `(file)` returns a list with the current line number and current file name 
being parsed.
- The primitive `(exit)` returns to the top-level loop.
- The variable `current-environment` contains the current environment.
- The primitive `(eval expr env)` evaluates `expr` in the environment `env`, for instance
`(eval '(plus 1 2) current-environment)` ⇒ 3.  If the environment is not supplied, 
the interpreter tries to use the implicit environment `$env` which is available inside
an `nlambda`-expression (see below).  If `$env` is not available, the current environment is used.

# Macros

A simple macro mechanism is now available.  Scheme uses a rather fancy macro system
which I considered too complicated for my purposes.  Instead, I implemented `nlambda`
which behaves like `lambda` except that the arguments are not evaluated. Together with
`eval`, this makes it possible to define the special forms `and`, `or` and `cond` within
the language itself.  Remember that `(and e1 e2)` should not evaluate `e2` if `e1` evaluates
to `#f`.  A naive implementation 

```
(define and
  (lambda (e1 e2)
    (if (e1) e2 #f)))
```

will not do because both arguments to `and` are evaluated before the `if` is evaluated,
thus causing `(and #f ((lambda (x) (x x)) (lambda (x) (x x))))` to loop forever rather
than just returning `#f`. (The second argument is an infinite loop.)

With a macro we can delay the evaluation of the arguments until they are needed.

```
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
```

Here, `and` accepts an unevaluated list of arguments `xs` and passes it on to `and$` together
with the implicit environment `$env` which is a special value bound to the caller's environment.
If `eval` (inside `and$`) didn't use this environment, the macro would be evaluated in the
environment of `and` which is not what we want.

# First-class continuations

The interpreter now supports `call/cc` (also available as `call-with-current-continuation`),
which provides first-class continuations. A continuation represents "the rest of the computation" -
what to do with a value once it's been computed.

## Basic usage

The simplest use of `call/cc` is to escape from a computation:

```scheme
(call/cc (lambda (k) (+ 5 (k 10))))  ⇒ 10
```

Here, `k` is the continuation - a procedure that, when invoked, abandons the current
computation (the `+ 5`) and returns its argument (10) as the result of the entire `call/cc`
expression.

If the continuation is never invoked, `call/cc` behaves normally:

```scheme
(call/cc (lambda (k) (+ 5 10)))  ⇒ 15
```

## Storing and invoking continuations

Continuations are first-class values and can be stored and invoked multiple times:

```scheme
(define saved-cont #f)

;; First time: capture the continuation and return 5
(define result (+ 100 (call/cc (lambda (k)
                                  (set! saved-cont k)
                                  5))))
;; result is 105 (100 + 5)

;; Later: invoke the saved continuation with a different value
(saved-cont 20)
;; This jumps back to the (+ 100 ...) expression and returns 120 (100 + 20)
```

When you invoke a stored continuation, execution jumps back to the point where the continuation
was captured, as if `call/cc` had returned the new value instead of the original one.

## Implementation

Continuations are implemented as first-class objects containing:
- The continuation register (where to return)
- A deep copy of the stack (all pending operations)

When `call/cc` is invoked, it:
1. Captures the current continuation state
2. Packages it as a procedure-like object
3. Calls the provided function with this continuation object

When a captured continuation is invoked, it:
1. Restores the saved continuation register
2. Restores the saved stack
3. Jumps to the saved continuation with the provided value

The key challenge is ensuring that captured continuations contain independent copies of all
mutable data structures (particularly argument lists on the stack). The implementation uses
deep copying to avoid sharing mutable state between the current computation and captured
continuations.


# Implementation details

While writing the interpreter I was faced with several difficulties which
caused a few days of pondering.  Below I describe a few decisions I had
to make.

## NaN boxing

The Lisp interpreter now uses NaN boxing (see references at the end) to store
different datatypes in a IEEE 754 NaN value.  The implementation details are 
described in a separate paper [https://github.com/kjepo/lisp/blob/main/nan-boxing.pdf].
The added benefit is that the Lisp interpreter now uses double
values internally (although they are shown as integers if the decimal part is zero).


## Parsing 

Parsing S-expressions is usually part of a computer science undergrad course.
In theory, a recursive descent parser sounds simple enough but a few 
additional items on the wish list makes it more difficult:

- S-expressions spread out of several lines:

```
[1] (car 
[2]  '(1 2 3))
;Value: 1
```

- Several S-expressions on one line:

```
[3] 314 #t 'quux (map zero? '(0 1 2)) (cdr '(1 . 2))
;Value: 314
;Value: #t
;Value: quux
;Value: (#t #f #f)
;Value: 2
```

- Incorporating GNU's readline with line editing, history, completion, etc.

This Lisp-parser is constructed as follows:

- At the top is a read-eval-print loop which calls `scan()`, `parse()` 
and `eval()` in an infinite loop. 
The user can quit with control-D or by typing `:quit`.

- `scan()` reads the next token from input by calling `nextrealchar()`
and `nextchar()` to get the next character.  `nextrealchar()` is just
a convenience function for `nextchar()` that skips blanks and tabs.

- `nextchar()` returns the next character in the buffer, but if the buffer
is empty, or a ';' is encountered, it calls `cr_readline()` to get another
line from input.

- `cr_readline()` calls GNU's readline, if we're reading from standard input,
or the POSIX readline if we're reading from a file. For GNU's readline
we append a newline character, otherwise we wouldn't be able to distinguish
between 
```
(car '(1 2
3))
```
and
```
(car '(1 23))
```

Also, `cr_readline()` updates the line number counter, prints a prompt, 
and adds the next line to the command history. It also handles interpreter
commands like `:help`, `:quit`, `:gc`, etc.

- Coming back to `scan`, it returns the next `token` which can be
end-of-line (`END`), an identifier (`ID`), a number (`NUM`), a string (`STR`), 
parenthesis (`LPAR` and `RPAR`), a dot (`DOT`), or a quote mark (`TICK`).
Normally, a scanner wouldn't return end-of-line but as mentioned before we
need to handle expressions spread over several lines so I found it essential
to include this token in the grammar.

- The grammar, finally, is

```
expr : atom 
     | "'" expr 
     | "(" ")" 
     | "(" seq 

seq  : ")" 
     | "." atom ")"
     | END seq 
     | expr seq 

atom : ID 
     | NUM 
     | STR 
```
The above grammar can easily be turned into a top-down recursive descent parser
with one token look-ahead.  It doesn't allow certain constructs, for instance
writing `()` across two lines, or placing the cdr of a dotted pair on the next
line - these could be accomodated in the grammar at the cost of additional 
productions but I don't deem them necessary for serious use so I leave that
as an exercise for the reader :-)

## Garbage collection

This Lisp uses Cheney's Stop-and-copy algorithm for garbage collection (file `gc.c`).
An alternative would have been Mark-and-sweep.  Let's give a brief overview of
both algorithms, along with their pros and cons:

- Stop-and-copy divides the available memory into two halves.  When the first half is full,
reachable objects are moved into the empty second half.  What remains in the first half is
non-reachable objects which represents old cells that no longer are used.  The first half
is wiped clean, the two halves are swapped and execution continues.

During stop-and-copy garbage collection the reachable objects are placed adjacent 
to each other in the second half.  Packing objects together leads to fewer page faults,
but by actually moving the objects, i.e., changing their addresses,  we can get into 
trouble, as we will discuss in a minute.

- Mark-and-sweep, if implemented correctly needs a complicated pointer reversal algorithm to
run in O(1) space. Also, free cons cells must be kept in a linked list as objects don't move.
In contrast, with Stop-and-copy, we have a large contiguous free memory area where larger
objects can be allocated without fragmentation problems.
But the fact that objects don't move in Mark-and-sweep is very useful and will 
avoid some of the problems discussed next.

Debugging garbage collection is a really hard: if there's a bug in the actual garbage collector,
the memory will be thrashed and there's no journal of what happened.  And even if the stop-and-copy
garbage collector runs correctly, the rest of the code must take into consideration that any 
reference to memory will be stale afterwards. 

At the heart of the interpreter is `cons()` which creates one (1) cons cell. 
If `cons()` notices that it is out of memory, it invokes the garbage collector. 

```
Obj cons(Obj car_, Obj cdr_) {
  thecars[free_index] = car_;
  thecdrs[free_index] = cdr_;
  conscell = mkpointer(free_index);
  if (++free_index >= MEMSIZE)
    gc();
  return conscell;
}
```

Thus, every time we call a function that (transitively) calls `cons`, garbage collection 
can be triggered. Therefore, code like this is highly dangerous:

```
void foo(Obj p) {
  Obj r = cons(...);
}
```
If the call to `cons` triggers garbage collection we can probably say goodbye to `p`!
Even calling `cons(x,y)` means that you that you can't use `x` and `y` afterwards.

Enter the *root set*.

The root set is a set of global variables which we can find after garbage collection.
The stop-and-copy algorithms works by first placing the root set in the empty half.
It then follows all references from the root set, copying reachable objects into the empty half.

In our case, the root set is 
`env val unev argl proc expr prim_proc stack cont conscell tmp1 tmp2 tmp3`.
The first 10 objects in this list belong to the register machine (which is described in SICP).
The last four, `conscell`, `tmp1`, `tmp2` and `tmp3`, are global variables that we can use
to safely maintain a reference to an object, if garbage collection is suddenly invoked.

In detail, before garbage collection starts, the root set is placed at the beginning 
(index 1) of the new memory where live objects are later copied into. 
After garbage collection, when the dust has settled,
we can retrieve the root set variables again at index 1 in the new space. 

A word of warning: the variables `tmp1`, `tmp2` and `tmp3` are globals so you can't use them
in a recursive function.  By now, you should realize that the sudden invocation of the
stop-and-copy garbage collector makes it a bit tricky to write the interpreter because you
never know when the rug will be pulled under your feet!  Based on my limited experience
there are two methods (both useful) of dealing with this "threat":

- Write your interpreter code without recursion and use the `tmp` variables for any references.

- Call the function `gc_need(n)` to make sure that `n` cells can be allocated without the garbage
collector being invoked:

```
void gc_need(int n) {
  if (free_index + n >= MEMSIZE)
    gc();
  if (free_index + n >= MEMSIZE)
    error("Out of memory");
}
```

## Reading lines with readline

If you're compiling on a Windows machine, you may have to replace the
call to the POSIX getline with the following code:

```
input = malloc(512);
if (NULL == fgets(input, 512, fp))
  input = "(exit)";
```



# Future plans

While I'm not aiming to write a fully fledged Scheme interpreter there
are a few things I'd like to do:

- Parser is not "GC safe".
- Add tab completion? [https://thoughtbot.com/blog/tab-completion-in-gnu-readline]
- When out of memory, the interpreter doesn't recover nicely (GC doesn't work).
- There's a bug when an error is reported: the line number shown is from the expression that initiated the call, and not the actual line where the error occurred.
- Parse command line arguments in a more conventional way so we don't have to write `lisp -h -q` but rather `lisp -hq`.
- (display expr) needs additional argument for how deep it should print
- Segmentation fault if you omit the last ')' in play (game.scm)
- In Scheme, `1+` can be an identifier.
- Allow (lambda (x. y) ...) as in other Lisp, where y is bound to the rest of the arguments.
- Introduce `#|` ... `|#` for comments

# References

Abelson and Sussman *Structure and Interpretation of Computer Programs*:
<br>
[https://mitpress.mit.edu/sites/default/files/sicp/index.html]

[https://www.nhplace.com/kent/Papers/Special-Forms.html]

[https://stackoverflow.com/questions/3465868/how-to-implement-a-lisp-macro-system]

[https://piotrduperas.com/posts/nan-boxing]

[https://craftinginterpreters.com/optimization.html]

[https://leonardschuetz.ch/blog/nan-boxing/]

[https://www.bcl.hamilton.ie/~barak/teach/F97/CS257/macros.html]

[https://blog.veitheller.de/Scheme_Macros_III:_Defining_let.html]

[https://old.reddit.com/r/ProgrammingLanguages/comments/1092p82/another_nanbased_tagging_strategy_for_dynamic/]
