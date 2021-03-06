 /**
  * A simple explicit-control, garbage-collecting    _    _
  * register-machine Scheme-like Lisp interpreter   | |  (_)____ __
  * written in 2022 by Kjell Post, but based on a   | |__| (_-< '_ \
  * dormant project from a college course in 1986   |____|_/__/ .__/
  *                                                           |_|
 **/

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <signal.h>
#include <unistd.h>
#include <ctype.h>
#include <math.h>
#include <string.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <assert.h>
#include "lisp.h"
#include "print.h"
#include "hashtab.h"
#include "gc.h"

Obj NIL = MAKE_SEXPR(0);
Obj parse_error = MAKE_SEXPR(-1);
int free_index = 1;
Obj True, False, env, val, unev, argl, proc, expr, stack;
Obj conscell, tmp1, tmp2, tmp3, prim_proc, cont;
Continuation label;

int verbose = 0;
char *fname;
FILE *fp;
int lineno;
char *progname;
char *input;
static jmp_buf jmpbuf;

void error(char *s)   {
  printf("%s on line %d, file %s\n", s, (lineno-1), fname);
  longjmp(jmpbuf, 1);
}

void interrupt_handler(int sig) {
  char c;
  signal(sig, SIG_IGN);
  printf("Interrupt\n");
  signal(SIGINT, interrupt_handler);
  longjmp(jmpbuf, 1);
}

void unbound(Obj id) {
  printf("Unbound variable %s at line %d, file %s\n", find(NAN_VALUE(id)), lineno, fname);
  longjmp(jmpbuf, 1);
}

Obj cons(Obj car_, Obj cdr_) {
  thecars[free_index] = car_;
  thecdrs[free_index] = cdr_;
  conscell = (Obj) MAKE_SEXPR(free_index);
  if (++free_index >= MEMSIZE && gc()) {
    fprintf(stderr, "Sorry - memory is full, even after GC\n");
    exit(1);
  }
  return conscell;
}

typedef enum {
  PRIM_CAR, PRIM_CDR, PRIM_CONS, PRIM_PAIRP, PRIM_PLUS, PRIM_MINUS, PRIM_TIMES, PRIM_DIV,
  PRIM_EQP, PRIM_LT,  PRIM_GT, PRIM_DISPLAY, PRIM_TRUNCATE, PRIM_NUMBERP, PRIM_SYMBOLP,
  PRIM_STRINGP, PRIM_BOOLEANP, PRIM_NULLP, PRIM_EXIT, PRIM_FILE, PRIM_EVAL } Primitive;

Obj mksym(char *id) { return (Obj) MAKE_SYMBOL(lookup(id));  }
Obj mknum(double n) { return (Obj) MAKE_DOUBLE(n); }
Obj mkstr(char *str) { return (Obj) MAKE_STR(lookup(str)); }
Obj mkprim(int n) { return (Obj) MAKE_PRIM(n); }

void push(Obj x) { stack = cons(x, stack); }

Obj pop() {
  if (EQ(stack, NIL)) {
    error("Stack underflow!");
    return NIL; 		/* not reached */
  } else {
    Obj x = car(stack);
    stack = cdr(stack);
    return x;
  }
}

void gc_need(int n) {
  if (free_index + n >= MEMSIZE)
    gc();
  if (free_index + n >= MEMSIZE)
    error("Out of memory");
}

// a procedure is a quadruple (PROCEDURE parameters body env)
Obj mkproc(Obj parameters, Obj body, Obj env) {
  tmp1 = env;  tmp2 = body;  tmp3 = parameters;
  gc_need(5);
  parameters = tmp3; body = tmp2; env = tmp1;
  return cons(PROCEDURE_SYM, cons(parameters, cons(body, cons(env, NIL))));
}

// a macro is a triple (MACRO parameters body) - env is not used
Obj mkmacro(Obj parameters, Obj body) {
  tmp2 = body;  tmp3 = parameters;
  gc_need(5);
  parameters = tmp3; body = tmp2;
  return cons(MACRO_SYM, cons(parameters, cons(body, NIL)));
}

// destructive append, sets the cdr of l to m (unless l is NIL of course)
Obj concat(Obj l, Obj m) {
  Obj head = l;
  if (EQ(l, NIL))
    return m;
  while (!EQ(cdr(l), NIL))
    l = cdr(l);
  cdr(l) = m;
  return head;
}

Obj adjoin_arg(Obj arg, Obj arglist) {
  tmp1 = arg; tmp2 = arglist;
  gc_need(2);
  arg = tmp1; arglist = tmp2;
  return concat(arglist, cons(arg, NIL));
}

int length(Obj p) {
  int n = 0;
  while (!EQ(p, NIL)) {
    p = cdr(p);
    n++;
  }
  return n;
}

Obj pairup_aux(Obj vars, Obj vals) {
  if (EQ(vars, NIL))
    return NIL;
  else
    return cons(cons(car(vars), car(vals)), pairup_aux(cdr(vars), cdr(vals)));
}

Obj pairup(Obj vars, Obj vals) {
  if (EQ(vars, NIL))
    return NIL;
  int n = length(vars);
  int m = length(vals);
  if (m != n) {
    display(vars); printf(" "); display(vals); NL;
    error("Error in pairup: length mismatch between parameters and arguments");
    return NIL; 		// not reached
  } else {
    tmp1 = vars;
    tmp2 = vals;
    gc_need(2*n);
    return pairup_aux(tmp1, tmp2);
  }
}

Obj bind(Obj vars, Obj vals, Obj environment) {
  if (!IS_SEXPR(vars)) {        // ((lambda l body) '(1 2 3)) => l/'(1 2 3)
    tmp3 = environment;
    tmp1 = cons(vars, NIL);
    tmp2 = cons(vals, NIL);
    tmp2 = pairup(tmp1, tmp2);
    return concat(tmp2, tmp3);
  } else {        // ((lambda (x y z) body) '(1 2 3)) => bind x/1, y/2, z/3
    tmp3 = environment;
    tmp1 = pairup(vars, vals);	// <--- uses tmp1 and tmp2
    environment = tmp3;
    return concat(tmp1, environment);
  }
}

void prepend(Obj x, Obj l) {
  Obj first = car(l);
  Obj rest = cdr(l);
  tmp1 = x; tmp2 = l;
  conscell = cons(first, rest);
  x = tmp1; l = tmp2;
  car(l) = x;
  cdr(l) = conscell;
}

void add_binding(Obj var, Obj val, Obj env) {
  tmp1 = env;
  tmp2 = cons(var, val);
  prepend(tmp2, tmp1);
}

// find the first pair in env whose car equals var and return that pair
Obj assoc(Obj var, Obj env) {
  if (EQ(env, NIL))
    return NIL;
  else if (EQ(car(env), NIL))
    return NIL;
  else if (EQ(car(car(env)), var))
    return car(env);
  else
    return assoc(var, cdr(env));
}

Obj env_lookup(Obj var, Obj env) {      // env[var]
  Obj pair = assoc(var, env);
  if (EQ(pair, NIL)) {
    unbound(var);
    return NIL;
  } else
    return cdr(pair);
}

void set_variable_value(Obj var, Obj val, Obj env) {
  Obj pair = assoc(var, env);
  if (EQ(pair, NIL))
    unbound(var);
  else
    cdr(pair) = val;
}

void define_variable(Obj var, Obj val, Obj env) {
  Obj pair = assoc(var, env);
  if (!EQ(pair, NIL))
    cdr(pair) = val;            // overwrite old definition
  else
    add_binding(var, val, env);
}

void init_symbols() {
  False = mksym("#f"); True = mksym("#t");
  IF_SYM = mksym("if");  EQ_SYM = mksym("eq");  ADD_SYM = mksym("add");
  SUB_SYM = mksym("sub");  MUL_SYM = mksym("mul");  DIV_SYM = mksym("div");
  CAR_SYM = mksym("car");  CDR_SYM = mksym("cdr");  CONS_SYM = mksym("cons");
  QUOTE_SYM = mksym("quote"); DEFINE_SYM = mksym("define");
  LAMBDA_SYM = mksym("lambda");  NLAMBDA_SYM = mksym("nlambda");
  SETBANG_SYM = mksym("set!");  BEGIN_SYM = mksym("begin");
  PROCEDURE_SYM = mksym("procedure");  MACRO_SYM = mksym("macro");
  val = unev = argl = proc = stack = NIL;
  cont = mknum(EVAL_DISPATCH);
}

void init_env() {
  // an environment BINDING id -> value is represented as cons(id, value)
  // an ENVIRONMENT is a list of bindings ( ( id . value ) ( id . value ) ... )
  prim_proc = cons(NIL, NIL);  // can't be NIL (make space for 1 binding)
  add_binding(mksym("cons"), mkprim(PRIM_CONS), prim_proc);
  add_binding(mksym("display"), mkprim(PRIM_DISPLAY), prim_proc);
  add_binding(mksym("#f"), False, prim_proc);
  add_binding(mksym("#t"), True, prim_proc);
  add_binding(mksym("car"), mkprim(PRIM_CAR), prim_proc);
  add_binding(mksym("cdr"), mkprim(PRIM_CDR), prim_proc);
  add_binding(mksym("pair?"), mkprim(PRIM_PAIRP), prim_proc);
  add_binding(mksym("plus"), mkprim(PRIM_PLUS), prim_proc);
  add_binding(mksym("minus"), mkprim(PRIM_MINUS), prim_proc);
  add_binding(mksym("times"), mkprim(PRIM_TIMES), prim_proc);
  add_binding(mksym("div"), mkprim(PRIM_DIV), prim_proc);
  add_binding(mksym("eq?"), mkprim(PRIM_EQP), prim_proc);
  add_binding(mksym("<"), mkprim(PRIM_LT), prim_proc);
  add_binding(mksym(">"), mkprim(PRIM_GT), prim_proc);
  add_binding(mksym("truncate"), mkprim(PRIM_TRUNCATE), prim_proc);
  add_binding(mksym("number?"), mkprim(PRIM_NUMBERP), prim_proc);
  add_binding(mksym("symbol?"), mkprim(PRIM_SYMBOLP), prim_proc);
  add_binding(mksym("string?"), mkprim(PRIM_STRINGP), prim_proc);
  add_binding(mksym("boolean?"), mkprim(PRIM_BOOLEANP), prim_proc);
  add_binding(mksym("null?"), mkprim(PRIM_NULLP), prim_proc);
  add_binding(mksym("exit"), mkprim(PRIM_EXIT), prim_proc);
  add_binding(mksym("file"), mkprim(PRIM_FILE), prim_proc);
  add_binding(mksym("eval"), mkprim(PRIM_EVAL), prim_proc);
  add_binding(mksym("current-environment"), prim_proc, prim_proc);
}

int is_variable()        { return IS_SYMBOL(expr); }
int is_compound(Obj p)   { return is_pair(p) && EQ(PROCEDURE_SYM, car(p)); }
int is_macro(Obj p)      { return is_pair(p) && EQ(MACRO_SYM, car(p)); }
int is_nil(Obj p)        { return EQ(p, NIL); }
int is_pair(Obj p)       { return IS_SEXPR(p) && !is_nil(p); }
int is_self_evaluating() { return is_nil(expr) || IS_DOUBLE(expr) || IS_STR(expr); }
int is_quote()           { return is_pair(expr) && EQ(QUOTE_SYM, car(expr)); }
int is_if()              { return is_pair(expr) && EQ(IF_SYM, car(expr)); }
int is_assignment()      { return is_pair(expr) && EQ(SETBANG_SYM, car(expr)); }
int is_definition()      { return is_pair(expr) && EQ(DEFINE_SYM, car(expr)); }
int is_lambda()          { return is_pair(expr) && EQ(LAMBDA_SYM, car(expr)); }
int is_nlambda()         { return is_pair(expr) && EQ(NLAMBDA_SYM, car(expr)); }
int is_begin()           { return is_pair(expr) && EQ(BEGIN_SYM, car(expr)); }
int is_application()     { return is_pair(expr); }

void checklist(Obj p, char *fcnname) {
  if (is_pair(p))
    return;
  printf("The object "); display(p);
  printf(" passed as the first argument of %s, is not of the correct type.\n", fcnname);
  printf("(Line %d, file %s)\n", lineno, fname);
  longjmp(jmpbuf, 1);
}

double checknr(Obj p, char *opname) {
  if (IS_DOUBLE(p))
    return DOUBLEVALUE(p);
  printf("The object "); display(p);
  printf(" passed as an argument of %s, is not of the correct type.\n", opname);
  longjmp(jmpbuf, 1);
}

// argl is a list of arguments (a1 a2 a3 .. aN)
// if the primitive function only takes one argument, it is a1; if it's two then it's a1 and a2
void prim_plus() { val = mknum(checknr(car(argl), "plus") + checknr(cadr(argl), "plus")); }
void prim_minus() { val = mknum(checknr(car(argl), "minus") - checknr(cadr(argl), "minus")); }
void prim_times() { val = mknum(checknr(car(argl), "times") * checknr(cadr(argl), "times")); }
void prim_div() { val = (Obj) MAKE_DOUBLE(checknr(car(argl), "div") / checknr(cadr(argl), "div")); }
void prim_lt() { val = checknr(car(argl), "<") < checknr(cadr(argl), "<") ? True : False; }
void prim_gt() { val = checknr(car(argl), ">") > checknr(cadr(argl), ">") ? True : False; }
void prim_eqp() {
  Obj x1 = car(argl), x2 = cadr(argl);
  val = x1.as_int == x2.as_int ? True : False;
}
void prim_car() { checklist(car(argl), "car"); val = car(car(argl)); }
void prim_cdr() { checklist(car(argl), "cdr"); val = cdr(car(argl)); }
void prim_cons() { val = cons(car(argl), cadr(argl)); }
void prim_pairp() { val = is_pair(car(argl)) ? True : False; }
void prim_nullp() { val = (is_nil(car(argl)) ? True : False); }
void prim_display() { display2(car(argl), 0, -10000); }
void prim_truncate() { val = mknum(trunc(checknr(car(argl), "truncate"))); }
void prim_numberp() { val = IS_DOUBLE(car(argl)) ? True : False; }
void prim_symbolp() { val = IS_SYMBOL(car(argl)) ? True : False; }
void prim_stringp() { val = IS_STR(car(argl)) ? True : False; }
void prim_booleanp() { val = (EQ(car(argl), True) || EQ(car(argl), False)) ? True : False; }
void prim_exit() { longjmp(jmpbuf, 1); }
void prim_file() { val = cons(mknum(lineno-1), cons(mkstr(fname), NIL)); }
void eval();
void prim_eval() {
  push(expr);
  push(env);
  expr = car(argl);
  if (length(argl) > 1)     // use supplied 2nd argument for environment
    env = cadr(argl);
  else {                        // if we're inside an nlambda, use $env
    Obj macroenv = assoc(mksym("$env"), env);
    if (!is_nil(macroenv))
      env = macroenv;
  } 
  eval();
  env = pop();
  expr = pop();
}

void (*primitives[])() = {
  prim_car, prim_cdr, prim_cons, prim_pairp, prim_plus, prim_minus, prim_times,
  prim_div, prim_eqp, prim_lt, prim_gt, prim_display, prim_truncate, prim_numberp,
  prim_symbolp, prim_stringp, prim_booleanp, prim_nullp, prim_exit, prim_file, prim_eval };

void eval() {			// evaluate expr in env
  label = EVAL_DISPATCH;
  cont = mknum(PRINT_RESULT);
  for (;;) {
    switch (label) {
    case EV_APPL_OPERAND_LOOP:
      display_registers("EV_APPL_OPERAND_LOOP");
      push(argl);
      expr = car(unev);                /* 1st operand */
      if (is_nil(cdr(unev))) {         /* last operand */
        label = EV_APPL_LAST_ARG;
      } else {
        push(env);
        push(unev);
        cont = mknum(EV_APPL_ACCUMULATE_ARG);
        label = EVAL_DISPATCH;
      }
      continue;

    case EV_APPL_LAST_ARG:
      display_registers("EV_APPL_LAST_ARG");
      cont = mknum(EV_APPL_ACCUM_LAST_ARG);
      label = EVAL_DISPATCH;
      continue;

    case EV_SEQUENCE:
      display_registers("EV_SEQUENCE");
      expr = car(unev);         /* first expression */
      if (is_nil(cdr(unev)))           /* last expression */
        label = EV_SEQUENCE_LAST_EXP;
      else {
        push(unev);
        push(env);
        cont = mknum(EV_SEQUENCE_CONTINUE);
        label = EVAL_DISPATCH;
      }
      continue;

    case EV_SEQUENCE_LAST_EXP:
      display_registers("EV_SEQUENCE_LAST_EXP");
      cont = pop();
      label = EVAL_DISPATCH;
      continue;

    case UNKNOWN_PROCEDURE_TYPE:
      display_registers("UNKNOWN_PROCEDURE_TYPE");
      display(proc); NL;
      error("unknown procedure");
      continue;

    case EV_SEQUENCE_CONTINUE:
      display_registers("EV_SEQUENCE_CONTINUE");
      env = pop();
      unev = pop();
      unev = cdr(unev);
      label = EV_SEQUENCE;
      continue;

    case EV_APPL_ACCUMULATE_ARG:
      display_registers("EV_APPL_ACCUMULATE_ARG");
      unev = pop();
      env = pop();
      argl = pop();
      argl = adjoin_arg(val, argl);
      unev = cdr(unev);         /* rest operands */
      label = EV_APPL_OPERAND_LOOP;
      continue;

    case EV_APPL_ACCUM_LAST_ARG:
      display_registers("EV_APPL_ACCUMULATE_LAST_ARG");
      argl = pop();
      argl = adjoin_arg(val, argl);
      proc = pop();
      label = APPLY_DISPATCH;
      continue;

    case EV_SELF_EVAL:
      display_registers("EV_SELF_EVAL");
      val = expr;
      label = DOUBLEVALUE(cont);
      continue;

    case EV_VARIABLE:
      display_registers("EV_VARIABLE");
      val = env_lookup(expr, env);
      label = DOUBLEVALUE(cont);
      continue;

    case EV_QUOTED:
      display_registers("EV_QUOTED");
      val = cadr(expr);
      label = DOUBLEVALUE(cont);
      continue;

    case EV_IF:
      display_registers("EV_IF");
      push(expr);               // save expression for later
      push(env);
      push(cont);
      cont = mknum(EV_IF_DECIDE);
      expr = cadr(expr);        // if-predicate
      label = EVAL_DISPATCH;
      continue;

    case EV_IF_DECIDE:
      display_registers("EV_IF_DECIDE");
      cont = pop();
      env = pop();
      expr = pop();
      label = (!EQ(val, False) ? EV_IF_CONSEQUENT : EV_IF_ALTERNATIVE);
      continue;

    case EV_IF_CONSEQUENT:
      display_registers("EV_IF_CONSEQUENT");
      expr = caddr(expr);       /* if-consequent */
      label = EVAL_DISPATCH;
      continue;

    case EV_IF_ALTERNATIVE:
      display_registers("EV_IF_ALTERNATIVE");
      if (!is_nil(cdddr(expr)))
        expr = cadddr(expr);
      else
        expr = NIL;
      label = EVAL_DISPATCH;
      continue;

    case EV_ASSIGNMENT:
      display_registers("EV_ASSIGNMENT");
      unev = cadr(expr);        // assignment variable
      push(unev);
      expr = caddr(expr);	// assignment value
      push(env);
      push(cont);
      cont = mknum(EV_ASSIGNMENT_1);
      label = EVAL_DISPATCH;
      continue;

    case EV_ASSIGNMENT_1:
      display_registers("EV_ASSIGNMENT_1");
      cont = pop();
      env = pop();
      unev = pop();
      set_variable_value(unev, val, env);
      label = DOUBLEVALUE(cont);
      continue;

    case EV_DEFINITION:
      display_registers("EV_DEFINITION");
      unev = cadr(expr);
      if (is_pair(unev)) {        // (define (name x1 x2 ...) body)
        unev = caadr(expr);       // name
        push(unev);
        gc_need(2);
        expr = cons(LAMBDA_SYM, cons(cdadr(expr), cddr(expr)));
      } else {                    // (define name expr)
        push(unev);
        expr = caddr(expr);       // expr
      }
      push(env);
      push(cont);
      cont = mknum(EV_DEFINITION_1);
      label = EVAL_DISPATCH;
      continue;

    case EV_DEFINITION_1:
      display_registers("EV_DEFINITION_1");
      cont = pop();
      env = pop();
      unev = pop();
      define_variable(unev, val, env);
      label = DOUBLEVALUE(cont);
      continue;

    case EV_LAMBDA:
      display_registers("EV_LAMBDA");
      unev = cadr(expr);        // parameters
      expr = cddr(expr);        // body
      val = mkproc(unev, expr, env);
      label = DOUBLEVALUE(cont);
      continue;

    case EV_NLAMBDA:
      display_registers("EV_NLAMBDA");
      unev = cadr(expr);        // parameters
      expr = cddr(expr);        // body
      val = mkmacro(unev, expr);  // env is not used
      label = DOUBLEVALUE(cont);
      continue;

    case EV_BEGIN:
      display_registers("EV_BEGIN");
      unev = cdr(expr);
      push(cont);
      label = EV_SEQUENCE;
      continue;

    case EV_APPLICATION:        // first evaluate operator
      display_registers("EV_APPLICATION");
      push(cont);
      push(env);
      unev = cdr(expr);
      push(unev);
      expr = car(expr);
      cont = mknum(EV_APPL_DID_OPERATOR);
      label = EVAL_DISPATCH;
      continue;

    case EV_APPL_DID_OPERATOR:
      display_registers("EV_APPL_DID_OPERATOR");
      unev = pop();
      env = pop();
      argl = NIL;
      proc = val;
      if (is_macro(proc))
        label = APPLY_DISPATCH; // macros don't evaluate arguments
      else if (is_nil(unev))
        label = APPLY_DISPATCH; // no arguments
      else {                    // evaluate arguments
        push(proc);
        label = EV_APPL_OPERAND_LOOP;
      }
      continue;

    case APPLY_DISPATCH:        // proc contains the evaluated operator
      display_registers("APPLY_DISPATCH");
      if (IS_PRIM(proc))
        label = PRIMITIVE_APPLY;
      else if (is_compound(proc))
        label = COMPOUND_APPLY;
      else if (is_macro(proc))
        label = MACRO_APPLY;
      else
        label = UNKNOWN_PROCEDURE_TYPE;
      continue;

    case PRIMITIVE_APPLY:       // proc contains the evaluated operator
      display_registers("PRIMITIVE_APPLY");
      primitives[NAN_VALUE(proc)]();
      cont = pop();
      label = DOUBLEVALUE(cont);
      continue;

    case COMPOUND_APPLY:        // proc = (PROCEDURE parameters body env)
      display_registers("COMPOUND_APPLY");
      unev = cadr(proc);           // procedure parameters
      env = cadddr(proc);          // procedure environment
      env = bind(unev, argl, env); // bind formals to arguments
      unev = caddr(proc);          // procedure body
      label = EV_SEQUENCE;
      continue;

    case MACRO_APPLY:           // proc = (MACRO parameters body)
      display_registers("MACRO_APPLY");
      env = bind(cadr(proc), unev, env);
      add_binding(mksym("$env"), env, env);
      unev = caddr(proc);
      label = EV_SEQUENCE;
      continue;

    case PRINT_RESULT:
      display_registers("PRINT_RESULT");
      return;

    case UNKNOWN_EXPRESSION_TYPE:
      printf("Unknown expression: ");
      display(expr); NL;
      val = NIL;
      return;

    case EVAL_DISPATCH:
      display_registers("EVAL_DISPATCH");
      if (is_self_evaluating()) {
        label = EV_SELF_EVAL;
      } else if (is_variable()) {
        label = EV_VARIABLE;
      } else if (is_quote()) {
        label = EV_QUOTED;
      } else if (is_if()) {
        label = EV_IF;
      } else if (is_assignment()) {
        label = EV_ASSIGNMENT;
      } else if (is_definition()) {
        label = EV_DEFINITION;
      } else if (is_lambda()) {
        label = EV_LAMBDA;
      } else if (is_nlambda()) {
        label = EV_NLAMBDA;
      } else if (is_begin()) {
        label = EV_BEGIN;
      } else if (is_application()) {
        label = EV_APPLICATION;
      } else {
        label = UNKNOWN_EXPRESSION_TYPE;
      }
      continue;
    default:
      printf("Illegal label: %d\n", (int) label);
      exit(1);
    }
  }
}

// scanner and parser for LISP-style input
typedef enum toktype {
  END=1, ID, NUM, STR, LPAR = '(', RPAR = ')', DOT = '.', TICK = '\'' } Token;

Token token;                    // current token
char id[80];                    // string value when token == ID
double nval;                       // numeric value when token == NUM

int legal_symbol_start(char ch) { return isalpha(ch) || strchr("#+-.*/<=>!?:$%_&~^", ch); }
int legal_symbol_rest(char ch)  { return isalnum(ch) || strchr("#+-.*/<=>!?:$%_&~^", ch); }

void cmd(char *line) {
  if (0 == strcmp(line, ":quit"))
    exit(0);
  else if (0 == strcmp(line, ":help")) {
    printf(":quit ==> quit (ctrl-D works too)\n");
    printf(":help ==> this help text\n");
    printf(":env  ==> print environment\n");
    printf(":word ==> print word size\n");
    printf(":mem  ==> show free memory\n");
    printf(":gc   ==> force garbage collection\n");
    printf(":sym  ==> print all symbols in hashtable\n");
  } else if (0 == strcmp(line, ":env")) {
    display(env); NL;
  } else if (0 == strcmp(line, ":word")) {
    printf("%lu\n", sizeof(Obj));
  } else if (0 == strcmp(line, ":mem")) {
    printf("%d/%d cells used\n", free_index, MEMSIZE);
  } else if (0 == strcmp(line, ":gc")) {
    gc();
  } else if (0 == strcmp(line, ":sym")) {
    dump_hashtab();
  } else
    printf("unknown command: %s\n", line);
}

void cr_readline() {
  char prompt[20], *p;
  size_t len;
  sprintf(prompt, "[%d] ", lineno++);
  if (fp != stdin) {
    input = NULL;
    if (getline(&input, &len, fp) == -1) // POSIX (but see README for options)
      input = "(exit)";
    return;
  }
  p = readline(prompt);
  if (!p)
    exit(0);
  add_history(p);
  if (p[0] == ':') {
    cmd(p);
    return cr_readline();
  }
  int n = strlen(p);
  input = malloc(n + 2);
  strcpy(input, p);		/* fixme: memory leak */
  input[n] = '\n';		/* add \n at end of input */
  input[n+1] = 0;
}

char nextchar() {
  while (input == 0 || *input == 0 || *input == ';')
    cr_readline();
  return *input++;
}

char nextrealchar() {
  char ch;
  do {
    ch = nextchar();
  } while (ch == ' ' || ch == '\t');
  return ch;
}

Token scan() {
  char *p, ch = nextrealchar(), lastchar;
  switch (ch) {
  case '\n':
    return token = END;
  case '\"':
    p = id;
    lastchar = ch;
    while ((ch = nextchar()) != EOF) {
      if (lastchar == '\\') {
        if (ch == 'a')	  *(p-1) = '\a';
        if (ch == 'b')	  *(p-1) = '\b';
        if (ch == 'f')	  *(p-1) = '\f';
        if (ch == 'n')	  *(p-1) = '\n';
        if (ch == 'r')	  *(p-1) = '\r';
        if (ch == 't')	  *(p-1) = '\t';
        if (ch == 'v')	  *(p-1) = '\v';
        if (ch == '\'')	  *(p-1) = '\'';
        if (ch == '\\')	  *(p-1) = '\\';
      } else if (ch == '\"') {
        *p = 0;
        return token = STR;
      } else {
        *p++ = ch;
      }
      lastchar = ch;
    }
    error("unterminated string");
  case '(':
  case '[':
    return token = LPAR;
  case ')':
  case ']':
    return token = RPAR;
  case TICK:
    return token = TICK;
  case DOT:
    return token = DOT;
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    p = id;
    do {
      *p++ = ch;
      ch = nextchar();
    } while (isdigit(ch) || ch == '.');
    *p = 0;
    input--;
    sscanf(id, "%lf", &nval);
    return token = NUM;
  default:
    if (legal_symbol_start(ch) && ch != ' ') {
      char cc = 0, *p = id;
      *p++ = ch;
      while ((ch = nextchar()) && legal_symbol_rest(ch))
        *p++ = ch;
      input--;
      *p = 0;
      // a integer number is an optional + or - followed by at least one or more digits,
      // with an optional fractional part.  These are numbers: 1, -1, +1, -12, -3.14
      // but these are not -, -1x, +, ++, .1
      if (sscanf(id, "%lf%c", &nval, &cc) == 1 && cc == 0)
        return token = NUM;
      return token = ID;
    } else {
      return scan();
    }
  }
}

Obj parse_atom() {
  if (token == ID) {
    return mksym(id);
  } else if (token == NUM) {
    return mknum(nval);
  } else if (token == STR) {
    return mkstr(id);
  } else {
    error("expected number or symbol.");
    return parse_error;			/* unreachable */
  }
}

Obj parse();
Obj parse_seq() {
  if (token == END) {		/* continue parsing past end-of-line */
    scan();
    return parse_seq();
  } else if (token == RPAR) {	/* end of list */
    return NIL;
  } else if (token == DOT) {	/* dotted pair, e.g., (1 2 . 3) */
    scan();
    Obj x = parse_atom();
    scan();
    if (token != ')')
      error("')' expected");
    return x;
  } else {			/* regular list, e.g., (1 2 3) */
    Obj x = parse();
    scan();
    return cons(x, parse_seq());
  }
}

Obj parse() {
  if (token == ID || token == NUM || token == STR) {
    return parse_atom();
  } else if (token == TICK) {
    scan();
    return cons(QUOTE_SYM, cons(parse(), NIL));
  } else if (token == LPAR) {
    scan();
    if (token == RPAR)
      return NIL;
    else
      return parse_seq();
  } else if (token == RPAR) {
    scan();
    return parse_error;			/* tell REPL to ignore */
  } else {
    error("expected number, symbol, or '('.");
    return parse_error;			/* tell REPL to ignore */
  }
}

void repl() {
  input = 0;
  lineno = 1;
  for (;;) {
    scan();
    if (strncmp(input, "exit)", 5) == 0 && fp != stdin)
      return;  /* kludge to stop reading from file */
    if (token == END)		// reached trailing \n
      continue;
    expr = parse();	      // printf("expr = "); display(expr); NL;
    if (EQ(expr, parse_error))	      /* skip extraneous right parenthesis */
      continue;
    env = prim_proc;
    eval();
    if (fp == stdin || verbose == -1) {
      printf(";Value: ");
      display(val); NL;
    }
  }
}

void slurp(char *f) {
  fname = f;
  if (!(fp = fopen(fname, "r"))) {
    fprintf(stderr, "%s: could not open %s\n", progname, fname);
    exit(1);
  } else {
    printf("reading %s...\n", fname);
    repl();
    fclose(fp);
    fp = stdin;
  }
}

int main(int argc, char *argv[]) {
  char *usage = "usage: %s [-q] [-h] [-v] [filename ...]\n";
  progname = argv[0];
  int libloaded = 0;

  assert(sizeof(double) == 8);
  assert(sizeof(Obj) == 8);

  printf("Welcome to Lisp, type \":help\" for help.\n");

  thecars = (Obj *)calloc(MEMSIZE, sizeof(Obj));
  thecdrs = (Obj *)calloc(MEMSIZE, sizeof(Obj));
  newcars = (Obj *)calloc(MEMSIZE, sizeof(Obj));
  newcdrs = (Obj *)calloc(MEMSIZE, sizeof(Obj));

  init_symbols();
  init_env();

  fp = stdin;
  fname = "stdin";
  signal(SIGINT, interrupt_handler);
  if (setjmp(jmpbuf) == 1)
    goto repl;

  for (int i = 1; i < argc; i++) {
    if (argv[i][0] == '-') {
      if (argv[i][1] == 'q') {
	libloaded = 1;
      } else if (argv[i][1] == 'v') {
        verbose = 1;
      } else if (argv[i][1] == 's') {
        verbose = -1;
      } else if (argv[i][1] == 'h') {
        printf(usage, progname);
	printf("  _    _           This Lisp interpreter accepts the following commands:\n");
	printf(" | |  (_)____ __     -q  dont' load lib.scm\n");
	printf(" | |__| (_-< '_ \\    -h  prints this help message\n");
	printf(" |____|_/__/ .__/    -v  verbose mode (prints registers, memory, symbols)\n");
	printf("           |_|     The source for this project is at github.com/kjepo/lisp\n");
	exit(0);
      } else {
        fprintf(stderr, usage, progname);
        exit(1);
      }
    } else {
      if (!libloaded) {
	slurp("lib.scm");
	libloaded = 1;
      }
      slurp(argv[i]);
      fp = stdin;
      fname = "stdin";
    }
  }

  if (!libloaded) {
    slurp("lib.scm");
    libloaded = 1;
  }

  lineno = 1;
 repl:
  fp = stdin;
  fname = "stdin";
  repl();
}
