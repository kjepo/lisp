/**
 **  Simple explicit-control register-machine Scheme-like interpreter
 **  Written in 2022 by Kjell Post (but a dormant project since 1986)
 **
 **/ 

/**
  * List of things to implement:
  *
  * Dotted pairs can't be read
  * Catch C-c to stop interpreter and return to REPL
  * Replace magic numbers in objval and mknum in terms of NUM_TAG
  * Write garbage collector
  * Special forms and, or, not that doesn't evaluate arguments unnecessarily
  * call/cc
  *
 **/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <setjmp.h>
#include <readline/readline.h>
#include "lisp.h"
#include "hashtab.h"

int verbose = 0;
FILE *fp;
char *progname;

static jmp_buf jmpbuf;
void error(char *s)   {
  printf("%s\n", s);
  longjmp(jmpbuf, 1);
}

void display(Obj);
void display2(Obj, int);


// Objects are tagged with 3 most significant bits xxx
// xxx11111 11111111 11111111 11111111
// number      000bbbbbb ... bbbbbbbb
// symbol      001bbbbbb ... bbbbbbbb
// pair        010bbbbbb ... bbbbbbbb
// procedure   011bbbbbb ... bbbbbbbb
// primitive   100bbbbbb ... bbbbbbbb
// string      101
// boolean     110bbbbbb ... bbbbbbbb
// array       111

// some of these numbers should not be changed, in particular PAIR_TAG and NUM_TAG
#define PAIR_TAG   0
#define SYMBOL_TAG 1
#define NUM_TAG    2
#define PROC_TAG   3
#define PRIM_TAG   4
#define STR_TAG    5
#define BOOL_TAG   6
#define ARRAY_TAG  7

int objtype(Obj n) {
  return (n >> 29) & 0x7;
}

void printBits(size_t const size, void const * const ptr);

int objval(Obj n) {
  int c = n & 0x1fffffff;  
  if (objtype(n) == NUM_TAG)
    if (n & 0x10000000) {       /* a negative number (2's complement) */
      return c | 0xe0000000;
    }
  return c;
}

void unbound(Obj id) {
  printf("Unbound variable %s\n", find(objval(id)));
  longjmp(jmpbuf, 1);
}

#define MEMSIZE 8192

Obj *thecars, *thecdrs, *newcars, *newcdrs;
Obj free_index = 1;                    // can't start at 0 because 0 means NIL
Obj NIL = 0;

#define car(p) (thecars[p])
#define cdr(p) (thecdrs[p])
#define cddr(p) (cdr(cdr(p)))
#define cdddr(p) (cdr(cdr(cdr(p))))
#define cadr(p) (car(cdr(p)))
#define caddr(p) (car(cdr(cdr(p))))
#define cadddr(p) (car(cdr(cdr(cdr(p)))))

Obj mknum(int n) { return (n & 0x1fffffff) | (NUM_TAG << 29); }
Obj mkstr(char *str) { return lookup(str) | (STR_TAG << 29); }
Obj mksym(char *id) { return lookup(id) | (SYMBOL_TAG << 29); }
Obj mkbool(char *id) { return lookup(id) | (BOOL_TAG << 29); }

Obj cons(Obj car_, Obj cdr_) {  /* aka mkpair */
  if (free_index > MEMSIZE)
      error("Error: memory full!");
  car(free_index) = car_;
  cdr(free_index) = cdr_;
  return free_index++ | (PAIR_TAG << 29);
}

Obj list(Obj p) { return cons(p, NIL); }

Obj append(Obj l, Obj m) { return (l == NIL ? m : cons(car(l), append(cdr(l), m))); }

Obj adjoin_arg(Obj arg, Obj arglist) { return append(arglist, list(arg)); }

Obj mkproc(Obj parameters, Obj body, Obj env) {
  /* a procedure is a triple (parameters body env) */
  Obj p = cons(parameters, cons(body, cons(env, NIL)));
  return objval(p) | (PROC_TAG << 29);
}

typedef enum {
  PRIM_CAR,  PRIM_CDR,  PRIM_CONS,  PRIM_PAIRP,  PRIM_PLUS,  PRIM_MINUS,  PRIM_TIMES,  PRIM_EQ, PRIM_EQP,
  PRIM_LT,  PRIM_GT, PRIM_DISPLAY, PRIM_NUMBERP, PRIM_SYMBOLP, PRIM_NULLP, PRIM_EXIT } Primitive;

Obj mkprim(Primitive p) { return p | (PRIM_TAG << 29); }

Obj TRUE_SYM, IF_SYM, EQ_SYM, LET_SYM, ADD_SYM, SUB_SYM,
  MUL_SYM, DIV_SYM, DEFINE_SYM, CAR_SYM, CDR_SYM, CONS_SYM, ATOM_SYM,
  QUOTE_SYM, LETREC_SYM, LAMBDA_SYM, SETBANG_SYM, BEGIN_SYM;

typedef enum {
  PRINT_RESULT,                 /* 0 */
  EV_IF_DECIDE,
  EV_IF_CONSEQUENT,
  EV_IF_ALTERNATIVE,
  EV_ASSIGNMENT_1,              
  EV_DEFINITION_1,              
  EV_APPL_DID_OPERATOR,         
  EV_APPL_ACCUMULATE_ARG,       
  EV_APPL_ACCUM_LAST_ARG,       
  EV_SEQUENCE_CONTINUE,         
  EVAL_DISPATCH,                /* 10 */
  EV_SELF_EVAL,                 
  EV_VARIABLE,                  
  EV_QUOTED,                    
  EV_IF,                        
  EV_ASSIGNMENT,                
  EV_DEFINITION,                
  EV_LAMBDA,                    
  EV_BEGIN,
  EV_APPLICATION,
  EV_APPL_OPERAND_LOOP,         /* 20 */
  EV_APPL_LAST_ARG,
  EV_SEQUENCE,
  EV_SEQUENCE_LAST_EXP,
  APPLY_DISPATCH,
  PRIMITIVE_APPLY,
  COMPOUND_APPLY,
  UNKNOWN_PROCEDURE_TYPE,  
  UNKNOWN_EXPRESSION_TYPE       /* 28 */
} Continuation;

char *continuation_string[] = {
  "PRINT_RESULT",
  "EV_IF_DECIDE",
  "EV_IF_CONSEQUENT",
  "EV_IF_ALTERNATIVE",
  "EV_ASSIGNMENT_1",
  "EV_DEFINITION_1",
  "EV_APPL_DID_OPERATOR",
  "EV_APPL_ACCUMULATE_ARG",
  "EV_APPL_ACCUM_LAST_ARG",
  "EV_SEQUENCE_CONTINUE",
  "EVAL_DISPATCH",
  "EV_SELF_EVAL",
  "EV_VARIABLE",
  "EV_QUOTED",
  "EV_IF",
  "EV_ASSIGNMENT",
  "EV_DEFINITION",
  "EV_LAMBDA",
  "EV_BEGIN",
  "EV_APPLICATION",
  "EV_APPL_OPERAND_LOOP",
  "EV_APPL_LAST_ARG",
  "EV_SEQUENCE",
  "EV_SEQUENCE_LAST_EXP",
  "APPLY_DISPATCH",
  "PRIMITIVE_APPLY",
  "COMPOUND_APPLY",
  "UNKNOWN_PROCEDURE_TYPE",
  "UNKNOWN_EXPRESSION_TYPE"
};

Continuation cont, label;
Obj Stack[100];
int StackPtr;
Obj env, val, unev, argl, proc, expr;
Obj True, False;

Obj pairup(Obj vars, Obj vals) {
  if (vars == NIL)
    return NIL;
  else
    return cons(cons(car(vars), car(vals)),
                pairup(cdr(vars), cdr(vals)));
}

Obj bind(Obj vars, Obj vals, Obj env) {
  if (objtype(vars) != PAIR_TAG)                // ((lambda l body) '(1 2 3)) => bind l/'(1 2 3)
    return append(pairup(cons(vars, NIL), cons(vals, NIL)), env);
  else                                          // ((lambda (x y z) body) '(1 2 3)) => bind x/1, y/2, z/3 
    return append(pairup(vars, vals), env);
}

Obj bind1(Obj var, Obj value, Obj env) {
  return cons(cons(var, value), env);
}

void prepend(Obj x, Obj l) {
  Obj first = car(l);
  Obj rest = cdr(l);
  car(l) = x;
  cdr(l) = cons(first, rest);
}

void add_binding(Obj var, Obj val, Obj env) {
  Obj v = cons(var, val);
  prepend(v, env);
}

// find the first pair in env whose car equals var and return that pair (or NIL, if not found)
Obj assoc(Obj var, Obj env) {
  if (env == NIL)
    return NIL;
  else if (car(car(env)) == var)
    return car(env);
  else
    return assoc(var, cdr(env));
}

Obj env_lookup(Obj var, Obj env) {      /* env[var] */
  Obj pair = assoc(var, env);
  if (pair == NIL) {
    unbound(var);
    return NIL;
  } else
    return cdr(pair);
}

void set_variable_value(Obj var, Obj val, Obj env) {
  Obj pair = assoc(var, env);
  if (pair == NIL)
    unbound(var);
  else
    cdr(pair) = val;
}

void define_variable(Obj var, Obj val, Obj env) {
  Obj pair = assoc(var, env);
  if (pair != NIL)
    cdr(pair) = val;            /* overwrite old definition */
  else
    add_binding(var, val, env);
}

void push(Obj value) {
  Stack[StackPtr++] = value;
  if (StackPtr > sizeof(Stack)/sizeof(Stack[0]))
    error("Stack overflow!");
}

Obj pop() {
  if (StackPtr <= 0)
    error("Stack underflow!");
  return Stack[--StackPtr];
}

void init_symbols() {
  False = mkbool("#f");
  True = mkbool("#t");

  TRUE_SYM = mksym("true");
  IF_SYM = mksym("if");
  EQ_SYM = mksym("eq");
  LET_SYM = mksym("let");
  ADD_SYM = mksym("add");
  SUB_SYM = mksym("sub");
  MUL_SYM = mksym("mul");
  DIV_SYM = mksym("div");
  CAR_SYM = mksym("car");
  CDR_SYM = mksym("cdr");
  CONS_SYM = mksym("cons");
  ATOM_SYM = mksym("atom");
  QUOTE_SYM = mksym("quote");
  LETREC_SYM = mksym("letrec");
  DEFINE_SYM = mksym("define");
  LAMBDA_SYM = mksym("lambda");
  SETBANG_SYM = mksym("set!");
  BEGIN_SYM = mksym("begin");
  cont = EVAL_DISPATCH;
  val = NIL;
  unev = NIL;
  argl = NIL;
  proc = NIL;
  StackPtr = 0;
}

Obj prim_proc;

void init_env() {
  // an environment BINDING id -> value is represented simply as cons(id, value)
  // an ENVIRONMENT is a list of bindings ( ( id . value ) ( id . value ) .... )
  prim_proc = NIL;
  prim_proc = bind1(mksym("#f"), False, prim_proc);
  prim_proc = bind1(mksym("#t"), True, prim_proc);
  prim_proc = bind1(mksym("car"), mkprim(PRIM_CAR), prim_proc);
  prim_proc = bind1(mksym("cdr"), mkprim(PRIM_CDR), prim_proc);
  prim_proc = bind1(mksym("cons"), mkprim(PRIM_CONS), prim_proc);
  prim_proc = bind1(mksym("pair?"), mkprim(PRIM_PAIRP), prim_proc);
  prim_proc = bind1(mksym("plus"), mkprim(PRIM_PLUS), prim_proc);
  prim_proc = bind1(mksym("minus"), mkprim(PRIM_MINUS), prim_proc);
  prim_proc = bind1(mksym("times"), mkprim(PRIM_TIMES), prim_proc);
  prim_proc = bind1(mksym("=="), mkprim(PRIM_EQ), prim_proc);  // ??
  prim_proc = bind1(mksym("eq?"), mkprim(PRIM_EQP), prim_proc);
  prim_proc = bind1(mksym("<"), mkprim(PRIM_LT), prim_proc);
  prim_proc = bind1(mksym(">"), mkprim(PRIM_GT), prim_proc);
  prim_proc = bind1(mksym("display"), mkprim(PRIM_DISPLAY), prim_proc);
  prim_proc = bind1(mksym("number?"), mkprim(PRIM_NUMBERP), prim_proc);
  prim_proc = bind1(mksym("symbol?"), mkprim(PRIM_SYMBOLP), prim_proc);
  prim_proc = bind1(mksym("null?"), mkprim(PRIM_NULLP), prim_proc);
  prim_proc = bind1(mksym("exit"), mkprim(PRIM_EXIT), prim_proc);
}

int is_num(Obj p)        { return NUM_TAG == objtype(p); }
int is_str(Obj p)        { return STR_TAG == objtype(p); }
int is_bool(Obj p)       { return BOOL_TAG == objtype(p); }
int is_variable()        { return SYMBOL_TAG == objtype(expr); }
int is_primitive(Obj p)  { return PRIM_TAG == objtype(p); }
int is_compound(Obj p)   { return PROC_TAG == objtype(p); }
int is_nil(Obj p)        { return p == NIL; }
int is_pair(Obj p)       { return PAIR_TAG == objtype(p) && !is_nil(p); }
int is_self_evaluating() { return is_nil(expr) || is_num(expr) || is_str(expr) || is_bool(expr); }
int is_quote()           { return is_pair(expr) && QUOTE_SYM == car(expr); }
int is_if()              { return is_pair(expr) && IF_SYM == car(expr); }
int is_assignment()      { return is_pair(expr) && SETBANG_SYM == car(expr); }
int is_definition()      { return is_pair(expr) && DEFINE_SYM == car(expr); }
int is_lambda()          { return is_pair(expr) && LAMBDA_SYM == car(expr); }
int is_begin()           { return is_pair(expr) && BEGIN_SYM == car(expr); }
int is_application()     { return is_pair(expr); }

void verify_list(Obj p, char *fcnname) {
  if (is_pair(p) && is_pair(car(p)))
    return;
  printf("The object "); display(car(p));
  printf(" passed as the first argument of %s, is not of the correct type.\n", fcnname);
  longjmp(jmpbuf, 1);
}

int ensure_numerical(Obj p, char *opname) {
  if (is_num(p))
    return objval(p);
  printf("The object "); display(p);
  printf(" passed as an argument of %s, is not of the correct type.\n", opname);
  longjmp(jmpbuf, 1);
}
void prim_plus()  { val = mknum(ensure_numerical(car(argl), "+") + ensure_numerical(cadr(argl), "+")); }
void prim_minus()  { val = mknum(ensure_numerical(car(argl), "-") - ensure_numerical(cadr(argl), "-")); }
void prim_times()  { val = mknum(ensure_numerical(car(argl), "*") * ensure_numerical(cadr(argl), "*")); }
void prim_eq()  { val = ensure_numerical(car(argl), "=") == ensure_numerical(cadr(argl), "=") ? True : False; }
void prim_lt()  { val = ensure_numerical(car(argl), "<") < ensure_numerical(cadr(argl), "<") ? True : False; }
void prim_gt()  { val = ensure_numerical(car(argl), ">") > ensure_numerical(cadr(argl), ">") ? True : False; }

/* argl is a list of arguments (a1 a2 a3 .. aN) */
/* if the primitive function only takes one argument, it is a1 */
/* if the primitive function takes two arguments, they are a1 and a2 */
void prim_eqp()   { val = car(argl) == cadr(argl) ? True : False; }
void prim_car()   { verify_list(argl, "car"); val = car(car(argl)); }
void prim_cdr()   { verify_list(argl, "cdr"); val = cdr(car(argl)); }
void prim_cons()  { val = cons(car(argl), cadr(argl)); }
void prim_pairp() { val = is_pair(car(argl)) ? True : False; }
void prim_nullp() { val = (car(argl) == NIL ? True : False); }
void prim_display() { display(car(argl)); }
void prim_numberp() { val = (objtype(car(argl)) == NUM_TAG ? True : False); }
void prim_symbolp() { val = (objtype(car(argl)) == SYMBOL_TAG ? True : False); }
void prim_exit()  { longjmp(jmpbuf, 1); } /* fixme: maybe call this quit() and let exit do exit(0) ? */

void (*primitives[])() = {
  prim_car, prim_cdr, prim_cons, prim_pairp, prim_plus, prim_minus, prim_times, prim_eq, prim_eqp,
  prim_lt, prim_gt, prim_display, prim_numberp, prim_symbolp, prim_nullp, prim_exit };

#include "debug.h"

void eval_dispatch() {
  label = EVAL_DISPATCH;
  cont = PRINT_RESULT;
  for (;;) {
    switch (label) {
    case EV_APPL_DID_OPERATOR:
      display_registers("EV_APPL_DID_OPERATOR");
      unev = pop();
      env = pop();
      argl = NIL;
      proc = val;
      if (unev == NIL)          /* no operands */
        label = APPLY_DISPATCH;
      else {
        push(proc);
        label = EV_APPL_OPERAND_LOOP;
      }
      continue;

    case EV_APPL_OPERAND_LOOP:
      display_registers("EV_APPL_OPERAND_LOOP");
      push(argl);
      expr = car(unev);         /* 1st operand */
      if (!cdr(unev)) {         /* last operand */
        label = EV_APPL_LAST_ARG;
      } else {
        push(env);
        push(unev);
        cont = EV_APPL_ACCUMULATE_ARG;
        label = EVAL_DISPATCH;
      }
      continue;

    case EV_APPL_LAST_ARG:
      display_registers("EV_APPL_LAST_ARG");
      cont = EV_APPL_ACCUM_LAST_ARG;
      label = EVAL_DISPATCH;
      continue;

    case APPLY_DISPATCH:
      display_registers("APPLY_DISPATCH");
      if (is_primitive(proc))
        label = PRIMITIVE_APPLY;
      else if (is_compound(proc))
        label = COMPOUND_APPLY;
      else
        label = UNKNOWN_PROCEDURE_TYPE;
      continue;

    case PRIMITIVE_APPLY:
      display_registers("PRIMITIVE_APPLY");
      primitives[objval(proc)]();
      cont = pop();
      label = cont;
      display_registers("PRIMITIVE_APPLY PROLOG");
      continue;

    case COMPOUND_APPLY:
      display_registers("COMPOUND_APPLY");
      unev = car(objval(proc));      /* procedure parameters */
      env = caddr(objval(proc));     /* procedure environment */
      env = bind(unev, argl, env);   /* bind formals to arguments */
      unev = cadr(objval(proc));     /* procedure body */
      label = EV_SEQUENCE;
      continue;

    case EV_SEQUENCE:
      display_registers("EV_SEQUENCE");
      expr = car(unev);         /* first expression */
      if (!cdr(unev))           /* last expression */
        label = EV_SEQUENCE_LAST_EXP;
      else {
        push(unev);
        push(env);
        cont = EV_SEQUENCE_CONTINUE;
        label = EVAL_DISPATCH;
      }
      continue;

    case EV_SEQUENCE_LAST_EXP:
      display_registers("EV_SEQUENCE_LAST_EXP");
      cont = pop();
      label = EVAL_DISPATCH;
      continue;

    case UNKNOWN_PROCEDURE_TYPE:
      printf("Unknown procedure\n");
      display(proc);
      NL;
      val = NIL;
      label = cont;
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
      label = cont;
      continue;

    case EV_VARIABLE:
      display_registers("EV_VARIABLE");
      val = env_lookup(expr, env);
      label = cont;
      continue;

    case EV_QUOTED:
      display_registers("EV_QUOTED");
      val = cadr(expr);
      label = cont;
      continue;

    case EV_IF:
      display_registers("EV_IF");
      push(expr);               /* save expression for later */
      push(env);
      push(cont);
      cont = EV_IF_DECIDE;
      expr = cadr(expr);        /* if-predicate */
      label = EVAL_DISPATCH;
      continue;

    case EV_IF_DECIDE:
      display_registers("EV_IF_DECIDE");
      cont = pop();
      env = pop();
      expr = pop();
      label = (val != False ? EV_IF_CONSEQUENT : EV_IF_ALTERNATIVE);
      continue;

    case EV_IF_CONSEQUENT:
      display_registers("EV_IF_CONSEQUENT");
      expr = caddr(expr);       /* if-consequent */
      label = EVAL_DISPATCH;
      continue;

    case EV_IF_ALTERNATIVE:
      display_registers("EV_IF_ALTERNATIVE");
      if (cdddr(expr))
        expr = cadddr(expr);
      else
        expr = NIL;
      label = EVAL_DISPATCH;
      continue;

    case EV_ASSIGNMENT:
      display_registers("EV_ASSIGNMENT");
      unev = cadr(expr);        /* assignment variable */
      push(unev);
      expr = caddr(expr); /* assignment value */
      push(env);
      push(cont);
      cont = EV_ASSIGNMENT_1;
      label = EVAL_DISPATCH;
      continue;

    case EV_ASSIGNMENT_1:
      display_registers("EV_ASSIGNMENT_1");
      cont = pop();
      env = pop();
      unev = pop();
      set_variable_value(unev, val, env);
      label = cont;
      continue;

    case EV_DEFINITION:
      display_registers("EV_DEFINITION");
      unev = cadr(expr);
      push(unev);
      expr = caddr(expr);
      push(env);
      push(cont);
      cont = EV_DEFINITION_1;
      label = EVAL_DISPATCH;
      continue;

    case EV_DEFINITION_1:
      display_registers("EV_DEFINITION_1");
      cont = pop();
      env = pop();
      unev = pop();
      define_variable(unev, val, env);
      label = cont;
      continue;

    case EV_LAMBDA:
      display_registers("EV_LAMBDA");
      unev = cadr(expr);        /* parameters */
      expr = cddr(expr);        /* body */
      val = mkproc(unev, expr, env);
      label = cont;
      continue;

    case EV_BEGIN:
      display_registers("EV_BEGIN");
      unev = cdr(expr);
      push(cont);
      label = EV_SEQUENCE;
      continue;

    case EV_APPLICATION:
      display_registers("EV_APPLICATION");
      push(cont);
      push(env);
      unev = cdr(expr);
      push(unev);
      expr = car(expr);
      cont = EV_APPL_DID_OPERATOR;
      label = EVAL_DISPATCH;
      continue;

    case PRINT_RESULT:
      if (fp == stdin) {
	display(val); NL;
      }
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

void eval() {
  //  display(expr); NL;
  eval_dispatch();
}

// scanner and parser for LISP-style input
typedef enum toktype {
  END, ID, NUM, STR, CR, LPAR = '(', RPAR = ')', DOT = '.', PLUS = '+', MINUS = '-', TICK = '\''
} Token;

Token token;                    // current token
char id[80];                    // string value when token == ID
int nval;                       // numeric value when token == NUM

int legal_symbol_start(char ch) { return isalpha(ch) || strchr("#+-.*/<=>!?:$%_&~^", ch); }   
int legal_symbol_rest(char ch) { return isalnum(ch) || strchr("#+-.*/<=>!?:$%_&~^", ch); }   

Token scan() {
  char *p;
  char ch, lastchar;

 start_scan:
  do {                          /* skip whitespace */
    if (EOF == (ch = fgetc(fp)))
      return token = END;
  } while (isspace(ch));

  if (ch == ';') {             /* ; means comment until end-of-line */
    do {
      if (EOF == (ch = fgetc(fp)))
        return token = END;
    } while (ch != '\n');
    goto start_scan;
  }

  switch (ch) {                 /* ch is not ";", whitespace or EOF */
  case '\"':
    p = id;
    lastchar = ch;  
    while ((ch = fgetc(fp)) != EOF) {
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
	return (token = STR);
      } else {
	*p++ = ch;
      }
      lastchar = ch;
    }
    error("Unterminated string.");
  case LPAR:
    return token = LPAR;
  case RPAR:
    return token = RPAR;
  case TICK:
    return token = TICK;
  case DOT:
    return token = DOT;
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    ungetc(ch, fp);
    fscanf(fp, "%d", &nval);
    return token = NUM;
  default:
    if (legal_symbol_start(ch) && ch != ' ') {
      char cc, *p = id;
      *p++ = ch;
      while ((ch = fgetc(fp)) && legal_symbol_rest(ch))
        *p++ = ch;
      ungetc(ch, fp);
      *p = 0;
      // a number is an optional + or - followed by at least one or more digits
      // these are numbers: 1, -1, +1, -12 but these are not -, -1x, +, ++
      if (sscanf(id, "%d%c", &nval, &cc) == 1 && cc == 0)
	return (token = NUM);
      return (token = ID);
    } else
      printf("lexical error: %c\n", ch);
    break;
  }
  return token = END;
}

void expect(Token tok, char *msg) {
  if (token == tok)
    scan();
  else
    error(msg);
}

Obj parse_atom() {
  Obj x;
  if (token == ID) {
    x = mksym(id);
  } else if (token == NUM) {
    x = mknum(nval);
  } else if (token == STR) {
    x = mkstr(id);
  } else {
    error("expected number or symbol.");
  }
  scan();
  return x;
}

Obj parse();

Obj parse_seq() {
  if (token != RPAR && token != END)
    return cons(parse(), parse_seq());
  else
    return NIL;
}

Obj parse() { 
  if (token == END) {
    return NIL;
  } else if (token == ID || token == NUM || token == STR) {
    return parse_atom();
  } else if (token == TICK) {
    scan();
    return cons(QUOTE_SYM, cons(parse(), NIL));
  } else if (token == LPAR) {
    scan();
    if (token == RPAR) {
      scan();			/* () */
      return NIL;
    } else {
      Obj r = parse_seq();
      expect(RPAR, "')' expected");
      return r;
    }
  } else {
    error("expected number, symbol, or '('.");
    return NIL;			/* never executed */
  }
}

void rep() {
  scan();
  while ((expr = parse())) {
    env = prim_proc;
    eval();
    if (fp == stdin)
      printf("===> ");
  }
}

void slurp(char *fname) {
  if (!(fp = fopen(fname, "r"))) {
    fprintf(stderr, "%s: could not open %s\n", progname, fname);
    exit(1);
  } else {
    printf("parsing %s...\n", fname);
    rep();
    fp = stdin;
  }
}

int main(int argc, char *argv[]) {
  char *usage = "usage: %s [-h] [-v] [filename ...]\n";
  progname = argv[0];
  int libloaded = 0;

  thecars = (Obj *)malloc(MEMSIZE * sizeof(Obj));
  thecdrs = (Obj *)malloc(MEMSIZE * sizeof(Obj));  
  newcars = (Obj *)malloc(MEMSIZE * sizeof(Obj));
  newcdrs = (Obj *)malloc(MEMSIZE * sizeof(Obj));

  init_symbols();
  init_env();
  fp = stdin;

  if (setjmp(jmpbuf) == 1)
    goto repl;

  for (int i = 1; i < argc; i++) {
    if (argv[i][0] == '-') {
      if (argv[i][1] == 'q') {
	libloaded = 1;
      } else if (argv[i][1] == 'v') {
        verbose = 1;
      } else if (argv[i][1] == 'h') {
        fprintf(stderr, usage, progname);
	fprintf(stderr, "\nThis LISP interpreter accepts the following commands:\n\n");
	fprintf(stderr, "-q    Don't load lib.scm.\n\n");
	fprintf(stderr, "-h    Prints this help message.\n\n");
	fprintf(stderr, "-v    Turns on verbose mode which prints machine registers, memory and symbol table.\n\n");
	fprintf(stderr, "The source for this project is at http://www.github.com/kjepo/lisp\n");
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
    }
  }

  if (!libloaded) {
    slurp("lib.scm");
    libloaded = 1;
  }

  /*
  printf("testing readline:\n");
  char* input = readline("prompt> ");
  printf("%s\n", input);
  */

 repl:
  fp = stdin;
  printf("===> ");
  rep();

  if (verbose) {
    dump_memory();
    printf("env: "); display(env); NL;
    dump_hashtab();
  }
}
