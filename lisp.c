#include <stdio.h>
#include <stdlib.h>
#include "hashtab.h"

#define NL printf("\n");

void error(char *s) {
  fprintf(stderr, "%s\n", s);
  exit(1);
}

// Objects are tagged 32-bit values

typedef uint32_t Obj;

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
  return n & (0x3fff);
}

int objtype(Obj n) {
  return (n >> 30) & 0x3;
}

#define MEMSIZE 2048

Obj thecars[MEMSIZE];
Obj thecdrs[MEMSIZE];
Obj free_index = 1;		       // can't start at 0 because 0 means NIL

Obj car(Obj expr) {  return thecars[expr]; }
Obj cdr(Obj expr) {  return thecdrs[expr]; }

Obj mknum(int n) {
  return n | (NUM_TAG << 30);
}

Obj mksym(char *id) {
  int n = lookup(id);
  return n | (SYMBOL_TAG << 30);
}

Obj cons(Obj car, Obj cdr) {	/* aka mkpair */
  if (free_index > MEMSIZE)
      error("Error: memory full!");
  thecars[free_index] = car;
  thecdrs[free_index] = cdr;
  return free_index++ | (PAIR_TAG << 30);
}

Obj mkproc(Obj parameters, Obj body, Obj env) {
  /* a procedure is a triple (parameters body env) */
  Obj p = cons(parameters, cons(body, env));
  return objval(p) | (PROC_TAG << 30);
}

// an environment BINDING id -> value is represented simply as cons(id, value)
// an ENVIRONMENT is a list of bindings ( ( id . value ) ( id . value ) .... )




Obj NIL, TRUE_SYM, IF_SYM, EQ_SYM, LET_SYM, ADD_SYM, SUB_SYM,
  MUL_SYM, DIV_SYM, DEFINE_SYM, CAR_SYM, CDR_SYM, CONS_SYM, ATOM_SYM,
  QUOTE_SYM, LETREC_SYM, LAMBDA_SYM;

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

Continuation cont;
Obj Stack[10];
int StackPtr;
Obj val, unev, argl, proc;
Obj expr;



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
  val = car(expr);
}

void (*primitives[])() = { prim_car };


void init() {

  // primitives
  



  NIL = mksym("NIL"); TRUE_SYM = mksym("TRUE"); IF_SYM = mksym("IF");
  EQ_SYM = mksym("EQ");   LET_SYM = mksym("LET"); ADD_SYM = mksym("ADD");
  SUB_SYM = mksym("SUB"); MUL_SYM = mksym("MUL"); DIV_SYM = mksym("DIV");
  CAR_SYM = mksym("CAR"); CDR_SYM = mksym("CDR");
  CONS_SYM = mksym("CONS"); ATOM_SYM = mksym("ATOM");
  QUOTE_SYM = mksym("QUOTE"); LETREC_SYM = mksym("LETREC");
  DEFINE_SYM = mksym("DEFINE"); LAMBDA_SYM = mksym("LAMBDA");
  cont = EVAL_DISPATCH;
  val = NIL;
  unev = NIL;
  argl = NIL;
  proc = NIL;
  StackPtr = 0;
  
}

void display(Obj expr);
void display2(Obj expr, int dotted) {
  switch (objtype(expr)) {
  case PAIR_TAG:
    if (expr == 0)
      printf("NULL");
    else {
      if (!dotted)
	printf("(");
      display(thecars[expr]);
      if (thecdrs[expr] != 0) {
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
  default:
    fprintf(stderr, "display2: can't happen: %d\n", objtype(expr));
    exit(1);
  }
}

void display(Obj expr) {
  display2(expr, 0);
}

int is_self_evaluating() { return NUM_TAG == objtype(expr); }
int is_variable()        { return SYMBOL_TAG == objtype(expr); }
int is_quote()           { return QUOTE_SYM == car(expr); }
int is_if()              { return 0; }
int is_assignment()      { return 0; }
int is_definition()      { return DEFINE_SYM == car(expr); }
int is_lambda()          { return 0; }
int is_begin()           { return 0; }
int is_application()     { return PAIR_TAG == objtype(expr); }


void eval_dispatch() {
  init();
  printf("expr = "); display(expr);
  for (;;) {
    //    printf("cont = %d\n", cont);
    //    printf("expr = "); display(expr); NL;
    switch (cont) {
    case EV_APPL_DID_OPERATOR:
      // ...
      break;
    case EVAL_DISPATCH:
      cont = PRINT_RESULT;
      if (is_self_evaluating()) {
	val = expr;
	continue;
      } else if (is_variable()) {
	// ...
	continue;
      } else if (is_quote()) {
	val = car(cdr(expr));
	continue;
      } else if (is_if()) {
	// ...
	continue;
      } else if (is_assignment()) {
	// ...
	continue;
      } else if (is_definition()) {
	unev = car(cdr(expr));
	push(unev);
	expr = car(cdr(cdr(expr)));
	//	push(env);
	push(cont);
	cont = EV_DEFINITION_1;
	continue;
      } else if (is_lambda()) {
	cont = EV_LAMBDA;
      } else if (is_begin()) {
	cont = EV_BEGIN;
      } else if (is_application()) {
	push(cont);
	push(env);
	unev = cdr(expr);
	push(unev);
	expr = car(expr);
	cont = EV_APPL_DID_OPERATOR;
	continue;
      } else
	cont = UNKNOWN_EXPRESSION_TYPE;
      break;
    case PRINT_RESULT:
      printf("\n===> ");
      display(val);
      NL;
      return;
      // (retfcn val))
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

void dump() {
  int i, last;
  for (last = MEMSIZE; last >= 0; last--)
    if (thecars[last])
      break;
  
  printf("      ");
  for (i = 1; i <= last; i++) {
    printf("%d:\t", i);
  }
  printf("\n");
  printf("CARS: ");
  for (i = 1; i <= last; i++) {
    dumpval(thecars[i]);
    printf("\t");
  }
  printf("\nCDRS: ");
  for (i = 1; i <= last; i++) {
    dumpval(thecdrs[i]);
    printf("\t");
  }
  printf("\n");
}



int main() {
  // dump();
  //  testhashtab();
  expr = mknum(42);
  eval_dispatch();
  expr = cons(QUOTE_SYM, cons(mknum(17), 0));
  eval_dispatch();
  Obj p = mkproc(mksym("x"), cons(mksym("x"), NIL), 99);
  printf("expr = %d %d\n", objval(p), objtype(p));
  expr = cons(p, NIL);
  
  dump();
  dumphashtab();

}
