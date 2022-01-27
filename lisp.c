#include <stdio.h>
#include <stdlib.h>
#include "hashtab.h"

#define NL printf("\n");

// Objects are tagged 32-bit values

typedef uint32_t Obj;

// Objects are tagged with 2 most significant bits
// number      00bbbbbb ... bbbbbbbb
// symbol        01bbbbbb ... bbbbbbbb
// pair        10bbbbbb ... bbbbbbbb
// procedure   11bbbbbb ... bbbbbbbb

#define PAIR_TAG 0
#define SYMBOL_TAG 1
#define NUM_TAG  2
#define PROC_TAG 3

Obj mknum(int n) {
  return n | (NUM_TAG << 30);
}

Obj mksym(char *id) {
  int n = lookup(id);
  return n | (SYMBOL_TAG << 30);
}

Obj mkproc(int n) {
  return n | (PROC_TAG << 30);
}

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

Obj cons(Obj car, Obj cdr) {
  if (free_index > MEMSIZE) {
      printf("Error: memory full!\n");
      return 0;
  }
      
  thecars[free_index] = car;
  thecdrs[free_index] = cdr;
  return free_index++;
}


Obj NIL_SYM, TRUE_SYM, IF_SYM, EQ_SYM, LET_SYM, ADD_SYM, SUB_SYM,
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


void init() {
  NIL_SYM = mksym("NIL"); TRUE_SYM = mksym("TRUE"); IF_SYM = mksym("IF");
  EQ_SYM = mksym("EQ");   LET_SYM = mksym("LET"); ADD_SYM = mksym("ADD");
  SUB_SYM = mksym("SUB"); MUL_SYM = mksym("MUL"); DIV_SYM = mksym("DIV");
  CAR_SYM = mksym("CAR"); CDR_SYM = mksym("CDR");
  CONS_SYM = mksym("CONS"); ATOM_SYM = mksym("ATOM");
  QUOTE_SYM = mksym("QUOTE"); LETREC_SYM = mksym("LETREC");
  DEFINE_SYM = mksym("DEFINE"); LAMBDA_SYM = mksym("LAMBDA");
  cont = EVAL_DISPATCH;
  val = NIL_SYM;
  unev = NIL_SYM;
  argl = NIL_SYM;
  proc = NIL_SYM;
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
int is_application()     { return 0; }


void eval_dispatch() {
  init();
  printf("expr = "); display(expr);
  for (;;) {
    //    printf("cont = %d\n", cont);
    //    printf("expr = "); display(expr); NL;
    switch (cont) {
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
	cont = EV_APPLICATION;
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

int main() {
  // dump();
  //  testhashtab();
  // dumphashtab();
  expr = mknum(42);
  eval_dispatch();
  expr = cons(QUOTE_SYM, cons(mknum(17), 0));
  eval_dispatch();
}
