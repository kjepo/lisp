#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "lisp.h"
#include "hashtab.h"

int verbose = 0;

void error(char *s)              { fprintf(stderr, "%s\n", s); exit(1); }

void display(Obj);
void display2(Obj, int);

// Objects are tagged with 3 most significant bits
// number      000bbbbbb ... bbbbbbbb
// symbol      001bbbbbb ... bbbbbbbb
// pair        010bbbbbb ... bbbbbbbb
// procedure   011bbbbbb ... bbbbbbbb
// primitive   100bbbbbb ... bbbbbbbb
// string      101
// float       110
// array       111

#define PAIR_TAG   0
#define SYMBOL_TAG 1
#define NUM_TAG    2
#define PROC_TAG   3
#define PRIM_TAG   4

int objval(Obj n) {
  return n & (0x1fff);
}

int objtype(Obj n) {
  return (n >> 29) & 0x7;
}

void unbound(Obj id) {
  fprintf(stderr, "Unbound variable %s\n", find(objval(id)));
}

#define MEMSIZE 2048

Obj thecars[MEMSIZE];
Obj thecdrs[MEMSIZE];
Obj free_index = 1;		       // can't start at 0 because 0 means NIL

Obj NIL = 0;

#define CAR(p) (thecars[p])
#define CDR(p) (thecdrs[p])

Obj mknum(int n) {
  return n | (NUM_TAG << 29);
}

Obj mksym(char *id) {
  int n = lookup(id);
  return n | (SYMBOL_TAG << 29);
}

Obj cons(Obj car_, Obj cdr_) {	/* aka mkpair */
  if (free_index > MEMSIZE)
      error("Error: memory full!");
  CAR(free_index) = car_;
  CDR(free_index) = cdr_;
  return free_index++ | (PAIR_TAG << 29);
}

Obj list(Obj p) {
  return cons(p, NIL);
}

Obj append(Obj l, Obj m) {
  if (l == NIL)
    return m;
  else
    return cons(CAR(l), append(CDR(l), m));
}

Obj adjoin_arg(Obj arg, Obj arglist) {
  return append(arglist, list(arg));
}






Obj mkproc(Obj parameters, Obj body, Obj env) {
  /* a procedure is a triple (parameters body env) */
  Obj p = cons(parameters, cons(body, env));
  return objval(p) | (PROC_TAG << 29);
}

typedef enum {
  PRIM_CAR,
  PRIM_CDR,
  PRIM_CONS
} Primitive;

Obj mkprim(Primitive p) {
  return p | (PRIM_TAG << 29);
}



Obj TRUE_SYM, IF_SYM, EQ_SYM, LET_SYM, ADD_SYM, SUB_SYM,
  MUL_SYM, DIV_SYM, DEFINE_SYM, CAR_SYM, CDR_SYM, CONS_SYM, ATOM_SYM,
  QUOTE_SYM, LETREC_SYM, LAMBDA_SYM, SETBANG_SYM;

typedef enum {
  PRINT_RESULT,			/* 0 */
  EV_IF_DECIDE,			/* 1 */
  EV_ASSIGNMENT_1,		/* 2 */
  EV_DEFINITION_1,		/* 3 */
  EV_APPL_DID_OPERATOR,		/* 4 */
  EV_APPL_ACCUMULATE_ARG,	/* 5 */
  EV_APPL_ACCUM_LAST_ARG, 	/* 6 */
  EV_SEQUENCE_CONTINUE,		/* 7 */
  EVAL_DISPATCH,		/* 8 */
  EV_SELF_EVAL,			/* 9 */
  EV_VARIABLE,			/* 10 */
  EV_QUOTED,			/* 11 */
  EV_IF,			/* 12 */
  EV_ASSIGNMENT,		/* 13 */
  EV_DEFINITION,		/* 14 */
  EV_LAMBDA,			/* 15 */
  EV_BEGIN,			/* 16 */
  EV_APPLICATION,		/* 17 */
  UNKNOWN_EXPRESSION_TYPE	/* 18 */
} Continuation;

char *continuation_string[] = {
  "PRINT_RESULT",
  "EV_IF_DECIDE",
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
  "UNKNOWN_EXPRESSION_TYPE"
};


Continuation cont;
Obj Stack[10];
int StackPtr;
Obj env, val, unev, argl, proc, expr;

Obj bind(Obj var, Obj value, Obj env) {
  return cons(cons(var, value), env);
}

// env[var->value]
void add_binding(Obj var, Obj value, Obj env0) {
  env = bind(var, value, env0);
}

// find the first pair in env whose car equals var and return that pair (or NIL, if not found)
Obj assoc(Obj var, Obj env) {
  if (env == 0)
    return 0;
  else if (CAR(CAR(env)) == var)
    return CAR(env);
  else
    return assoc(var, CDR(env));
}

Obj env_lookup(Obj var, Obj env) {	/* env[var] */
  Obj pair = assoc(var, env);
  if (pair == 0) {
    unbound(var);
    return 0;
  } else
    return CDR(pair);
}

void set_variable_value(Obj var, Obj val, Obj env) {
  Obj pair = assoc(var, env);
  if (pair == 0)
    unbound(var);
  else
    thecdrs[pair] = val;
}

void define_variable(Obj var, Obj val, Obj env) {
  Obj pair = assoc(var, env);
  if (pair != 0)
    thecdrs[pair] = val;
  else
    add_binding(var, val, env);
}


void push(Obj value) {
  Stack[StackPtr++] = value;
  if (StackPtr > sizeof(Stack)/sizeof(Stack[0])) {
    fprintf(stderr, "Stack overflow!\n");
    exit(1);
  }
}

Obj pop() {
  if (StackPtr <= 0) {
    fprintf(stderr, "Stack underflow!\n");
    exit(1);
  }
  return Stack[--StackPtr];
}

void prim_car() {
  printf("**** CAR ****");
  val = CAR(expr);
}

void prim_cdr() {
  printf("**** CDR ****");
  val = CDR(expr);
}

void (*primitives[])() = { prim_car, prim_cdr };

void init() {

  // primitives




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
  cont = EVAL_DISPATCH;
  val = NIL;
  unev = NIL;
  argl = NIL;
  proc = NIL;
  StackPtr = 0;

}


int is_self_evaluating() { return NUM_TAG == objtype(expr); }
int is_variable()        { return SYMBOL_TAG == objtype(expr); }
int is_quote()           { return QUOTE_SYM == CAR(expr); }
int is_if()              { return 0; }
int is_assignment()      { return SETBANG_SYM == CAR(expr); }
int is_definition()      { return DEFINE_SYM == CAR(expr); }
int is_lambda()          { return 0; }
int is_begin()           { return 0; }
int is_application()     { return PAIR_TAG == objtype(expr); }
int is_primitive(Obj proc) { return 1; }
int is_compound(Obj proc)  { return 0; }



void display2(Obj expr, int dotted) {
  switch (objtype(expr)) {
  case PAIR_TAG:
    if (expr == 0)
      printf("NIL");
    else {
      if (!dotted)
	printf("(");
      display(CAR(expr));
      if (CDR(expr) != 0) {
	printf(" ");
	if (objtype(thecdrs[expr]) != PAIR_TAG)
	  printf(". ");
	display2(thecdrs[expr], 1);
      }
      if (!dotted)
	printf(")");
    }
    break;
  case SYMBOL_TAG:
    printf("%s", find(objval(expr)));
    break;
  case NUM_TAG:
    printf("%d", objval(expr));
    break;
  case PROC_TAG:
    printf("%d", objval(expr));
    break;
  case PRIM_TAG:
    printf("%d", objval(expr));
    break;
  default:
    fprintf(stderr, "display2: can't happen: %d\n", objtype(expr));
    exit(1);
  }
}

void display(Obj expr) {
  display2(expr, 0);
}

void display_registers(char *where) {
  if (!verbose)
    return;
  printf("===============================%s=\n", where);
  printf("expr: "); display(expr); NL;
  printf("env:  "); display(env); NL;
  printf("cont: "); printf("%s\n", continuation_string[cont]);
  printf("val:  "); display(val); NL;
  printf("unev: "); display(unev); NL;
  printf("argl: "); display(argl); NL;
  printf("proc: "); display(proc); NL;
  printf("stack ");
  for (int i = 0; i < StackPtr; i++) {
    printf("[");
    display(Stack[i]);
    printf("] ");
  }
  NL;
  printf("========================================\n");
}

void eval_dispatch() {
  init();
  for (;;) {
    switch (cont) {
    case EV_APPL_DID_OPERATOR:
      display_registers("EV_APPL_DID_OPERATOR");
      unev = pop();
      env = pop();
      argl = NIL;
      proc = val;
      if (!unev)		/* no operands */
	goto apply_dispatch;
      push(proc);
      goto ev_appl_operand_loop;

    ev_appl_operand_loop:
      display_registers("EV_APPL_OPERATOR_LOOP");
      push(argl);
      expr = CAR(unev);		/* 1st operand */
      if (!CDR(unev))		/* last operand */
	goto ev_appl_last_arg;
      push(env);
      push(unev);
      cont = EV_APPL_ACCUMULATE_ARG;
      goto eval_dispatch;

    ev_appl_last_arg:
      display_registers("EV_APPL_LAST_ARG");
      cont = EV_APPL_ACCUM_LAST_ARG;
      goto eval_dispatch;

    apply_dispatch:
      display_registers("APPLY_DISPATCH");
      if (is_primitive(proc))
	goto primitive_apply;
      if (is_compound(proc))
	goto compound_apply;
      goto unknown_procedure_type;

    primitive_apply:
      display_registers("PRIMITIVE_APPLY");
      primitives[objval(proc)](argl);
      //      val = mknum(88);		/* replace */
      cont = pop();
      continue;

    compound_apply:
      display_registers("COMPOUND_APPLY");
      unev = CAR(CDR(proc));	/* procedure parameters */
      env = CAR(CDR(CDR(CDR(proc)))); /* procedure environment */
      env = bind(unev, argl, env);
      unev = CAR(CDR(CDR(proc)));
      goto ev_sequence;

    ev_sequence:
      display_registers("EV_SEQUENCE");
      expr = CAR(unev);		/* first expression */
      if (!CDR(unev))		/* last expression */
	goto ev_sequence_last_exp;
      push(unev);
      push(env);
      cont = EV_SEQUENCE_CONTINUE;
      goto eval_dispatch;

    ev_sequence_last_exp:
      display_registers("EV_SEQUENCE_LAST_EXP");
      cont = pop();
      goto eval_dispatch;

    unknown_procedure_type:
      fprintf(stderr, "Error: unknown procedure\n");
      display(proc);
      NL;
      val = NIL;
      continue;

    case EV_SEQUENCE_CONTINUE:
      display_registers("EV_SEQUENCE_CONTINUE");
      env = pop();
      unev = pop();
      unev = CDR(unev);
      goto ev_sequence;

    case EV_APPL_ACCUMULATE_ARG:
      display_registers("EV_APPL_ACCUMULATE_ARG");
      unev = pop();
      env = pop();
      argl = pop();
      argl = adjoin_arg(val, argl);
      unev = CDR(unev);		/* rest operands */
      goto ev_appl_operand_loop;

    case EV_APPL_ACCUM_LAST_ARG:
      display_registers("EV_APPL_ACCUMULATE_LAST_ARG");
      argl = pop();
      argl = adjoin_arg(val, argl);
      proc = pop();
      goto apply_dispatch;


    case EV_ASSIGNMENT_1:
      display_registers("EV_ASSIGNMENT_1");
      cont = pop();
      env = pop();
      unev = pop();
      set_variable_value(unev, val, env);
      break;
    case EV_DEFINITION_1:
      display_registers("EV_DEFINITION_1");
      cont = pop();
      env = pop();
      unev = pop();
      define_variable(unev, val, env);
      break;
    case EVAL_DISPATCH:
      cont = PRINT_RESULT;
    eval_dispatch:
      if (is_self_evaluating()) {
	display_registers("is_self_evaluating");
	val = expr;
	continue;
      } else if (is_variable()) {
	display_registers("is_variable");
	val = env_lookup(expr, env);
	continue;
      } else if (is_quote()) {
	display_registers("is_quote");
	val = CAR(CDR(expr));
	continue;
      } else if (is_if()) {
	display_registers("is_if");
	// ...
	continue;
      } else if (is_assignment()) {
	display_registers("is_assignment");
	unev = CAR(CDR(expr));
	push(unev);
	expr = CAR(CDR(CDR(expr)));
	push(env);
	push(cont);
	cont = EV_ASSIGNMENT_1;
	goto eval_dispatch;
      } else if (is_definition()) {
	display_registers("is_definition");
	unev = CAR(CDR(expr));
	push(unev);
	expr = CAR(CDR(CDR(expr)));
	push(env);
	push(cont);
	cont = EV_DEFINITION_1;
	goto eval_dispatch;
      } else if (is_lambda()) {
	display_registers("is_lambda");
	cont = EV_LAMBDA;
      } else if (is_begin()) {
	display_registers("is_begin");
	cont = EV_BEGIN;
      } else if (is_application()) {
	display_registers("is_application");
	push(cont);
	push(env);
	unev = CDR(expr);
	push(unev);
	expr = CAR(expr);
	cont = EV_APPL_DID_OPERATOR;
	goto eval_dispatch;
      } else {
	cont = UNKNOWN_EXPRESSION_TYPE;
      }
      break;
    case PRINT_RESULT:
      printf("\n===> ");
      display(val);
      NL;
      return;
    default:
      fprintf(stderr, "Illegal continuation: %d\n", (int) cont);
      exit(1);
    }
  }
}

void dumpval(Obj n) {
  switch (objtype(n)) {
  case PAIR_TAG:
    if (objval(n))
      printf("P%d ", objval(n));
    else
      printf("NIL");
    break;
  case NUM_TAG:
    printf("INT %d ", objval(n));
    break;
  case SYMBOL_TAG:
    printf("A%d ", objval(n));
    break;
  case PROC_TAG:
    printf("$%d ", objval(n));
    break;
  }
}

void dump_memory() {
  int i, last;
  for (last = MEMSIZE; last >= 0; last--)
    if (CAR(last))
      break;

  printf("      ");
  for (i = 1; i <= last; i++) {
    printf("%d:\t", i);
  }
  printf("\n");
  printf("CARS: ");
  for (i = 1; i <= last; i++) {
    dumpval(CAR(i));
    printf("\t");
  }
  printf("\nCDRS: ");
  for (i = 1; i <= last; i++) {
    dumpval(thecdrs[i]);
    printf("\t");
  }
  printf("\n");
}

void setup_environment() {
  // an environment BINDING id -> value is represented simply as cons(id, value)
  // an ENVIRONMENT is a list of bindings ( ( id . value ) ( id . value ) .... )

  env = 0;
  env = bind(mksym("x"), mknum(8), env);
  env = bind(mksym("y"), mknum(7), env);
  env = bind(mksym("l123"), cons(mknum(1), cons(mknum(2), cons(mknum(3), 0))), env);
  env = bind(mksym("car"), mkprim(PRIM_CAR), env);
  env = bind(mksym("cdr"), mkprim(PRIM_CDR), env);
}

void eval() {
  printf("expr = ");
  display(expr);
  NL;
  eval_dispatch();
}

void test() {
  // 42
#ifdef BIGTEST
  expr = mknum(42); eval();
  assert(objval(val) == 42);

  // (quote 17)
  expr = cons(QUOTE_SYM, cons(mknum(17), 0));  eval();
  assert(objval(val) == 17);

  // x
  expr = mksym("x"); eval();
  assert(objval(val) == 8);

  // (set! x 22)
  expr = cons(SETBANG_SYM, cons(mksym("x"), cons(mknum(22), 0))); eval();
  assert(objval(env_lookup(mksym("x"), env)) == 22);

  // (define z 29)
  expr = cons(DEFINE_SYM, cons(mksym("z"), cons(mknum(29), 0))); eval();
  assert(objval(env_lookup(mksym("z"), env)) == 29);
#endif

  expr = cons(CAR_SYM, cons(cons(QUOTE_SYM, cons(mknum(1), cons(mknum(2), cons(mknum(3), 0)))), 0));
  // expr = cons(CAR_SYM, cons(mksym("l123"), 0));
  eval();

  expr = cons(CDR_SYM, cons(cons(QUOTE_SYM, cons(mknum(1), cons(mknum(2), cons(mknum(3), 0)))), 0));
  eval();
  
  expr = mksym("l123");
  eval();


/*
  Obj p = mkproc(mksym("x"), cons(mksym("x"), NIL), 99);
  printf("expr = %d %d\n", objval(p), objtype(p));
  expr = cons(p, NIL);
  */
}

int main(int argc, char *argv[]) {
  for (int i = 1; i < argc; i++) {
    if (argv[i][0] == '-') {
      if (argv[i][1] == 'v') {
	verbose = 1;
      } else {
	fprintf(stderr, "usage: %s [-v]\n", argv[0]);
	exit(1);
      }
    }
  }

  init();
  setup_environment();

  test();
  if (verbose)
    dump_memory();
  printf("env: "); display(env); NL;
  if (verbose)
    dump_hashtab();

}
