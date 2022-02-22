# explicit Lisp

This is an exercise in building a Lisp machine in a series of steps
so that (ultimately) we _could_ write it in assembler.
Why? Yes, why do some people go out in the wilderness and survive
only with the help of a knife when they could sit in comfort at their
home, eat take-away food and watch TV?

The original code in `explicit.scm` is a register machine implementation
of a Lisp interpreter written in Scheme.
Rather than the usual recursive apply/eval description of the interpreter,
we have made the control and arguments explicit.
The code is based on SICP, chapter 5 (and forwards).

(If you are not familiar with the traditional eval/apply rendition of a
Lisp interpreter, please watch "The most beautiful program ever written"
by William Byrd, [https://www.youtube.com/watch?v=OyfBQmvr2Hc])

The next step is to write the same code in C (file `lisp.c`).
The code is not finished yet but a rudimentary interpreter is working
and can load the library file `lib.scm`.  Here is a small example:

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

The informal description of the language is as follows:
```
expr    : symbol | ( expr* ) | ' expr
symbol  : ID | NUMBER | STRING
```

where `ID` is an identifier (letters optionally followed by letters and digits, and
where letters also include the special characters `#+-.*/<=>!?:$%_&~^`,
`NUMBER` is an optional sign (`+` or `-`) followed by one or more digits,
and `STRING` is a `"`-enclosed string which can contain the usual escape
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
`number?`, `symbol?`, `eq?` and `file`.

# Primitives

- The primitive `(file)` returns a list with the current line number and current file name being parsed.




# Tagged pointers

The interpreter handles _objects_ which can be 
- pairs of objects, i.e., cons cells
- symbols, i.e., `foo`, `-1+`, or `set!`
- numbers, i.e., `42`, `-17` or `+3`
- primitives, i.e., `car`, `cdr`, `cons`, etc
- strings, i.e., `"hello world\n"`
- booleans, i.e., `#t` and `#f`
- arrays (not yet implemented)

These objects are stored in 32-bit integers, but to separate them we use 3 bits as a tag for the
different kinds of objects. The tag can be stored in the lower 3 bits or in the 3 most significant bits.
The trade-offs between these are described in the next section but I have opted for storing the 
tag in the lower 3 bits.

## Store the tag in the most significant bits

```
tttbbbbb bbbbbbbb bbbbbbbb bbbbbbbb
```

- To determine the type, we need to shift the value 29 bits to the right, `(n >> 29) & 7`.

- To use the value, we need to mask off the three most significant bits, i.e., `n & 0x1fffffff`.

For pairs, we use the tag `000` so that we can use the tagged value "as-is".  This saves some
time when chasing lists with `car` and `cdr`.

For integers, it gets complicated: a negative integer in 2's complement format must have its
most significant bits set to 1 again after removing the tag.

If the tag for numbers is `111` then 17 is stored as `11100000 00000000 00000000 00010001`
while -17 is stored as `11111111 11111111 11111111 11101111`. So in order to get the value 
back, we must do different things depending on whether the 28th bit (4th bit from the left)
is 0 or 1: if it is 0 then we are storing a positive number and we simply set bits 29-32 to 0
to turn it into 17 again.
But if the 28th bit is 1, then we are storing a negative number and we set bits 29-32 to 1
to turn it back to -17.
(If the tag is `111` there is no need for the last operation, but we still need to examine 
the 28th bit every time we need the integer value.) So the largest number we can store is 
268435455 which is stored as `11101111 11111111 11111111 11111111`. The smallest number we 
can store is -268435456 which is stored as `11110000 00000000 00000000 00000000`.

As it turns out, we still have to consider negative numbers if we store the tag in the least
significant bits.

## Store the tag in the least significant bits

```
bbbbbbbb bbbbbbbb bbbbbbbb bbbbbttt
```

- To determine the type, we need to mask off the first thre bits, i.e., `n & 7`.

- To use the value, we need to shift the tagged value three bits to right, i.e., `n >> 3`.

For pairs, we would have to shift the value three bits to the right before using it.  This is 
a small extra cost we need to pay every time we follow the car/cdr of a cons cell and 
makes traversing a list more time consuming.

For integers, we still have to add the three missing (most significant) bits for negative
numbers when we get rid of the tag by shifting.
We _could_ use `000` as the tag for numbers and apply addition, subtraction, etc on the 
tagged numbers but Lisp program typically don't do a lot of number crunching so we
reserve the tag `000` for pairs instead.

If you read this and have a better idea of tagging pointers, please let me know.


# Garbage collection

This Lisp uses Cheney's Stop-and-copy algorithm for garbage collection. 
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
  conscell = mkpair(free_index);
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

The root set is a list of global variables which we can find after garbage collection.
The stop-and-copy algorithms works by first placing the root set in the empty half.
It then follows all references from the root set, copying reachable objects into the empty half.

In our case, the root set is a list
`env val unev argl proc expr prim_proc stack cont conscell tmp1 tmp2 tmp3`.
The first 10 objects in this list belong to the register machine (which is described in SICP).
The last four, `conscell`, `tmp1`, `tmp2` and `tmp3`, are global variables that we can use
to safely maintain a reference to an object, if garbage collection is suddenly invoked.

In detail, before garbage collection starts, the root set is updated with the current
values of the variables above. The root set is then placed at the beginning (index 1) of the new memory 
where live objects are later copied into. After garbage collection, when the dust has settled,
we can retrieve the root set again at index 1 in the new space. 

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

# Future plans

While I'm not aiming to write a fully fledged Scheme interpreter there
are a few things I'd like to do:

- Catch C-c and use GNU's readline library to parse input from stdin.
- `call/cc`
- Special forms needed for `and`, `or`, `not`.
- Macros
- Add more primitives, for instance `let`, `cond`

# References

Abelson and Sussman *Structure and Interpretation of Computer Programs*:
<br>
https://mitpress.mit.edu/sites/default/files/sicp/index.html
