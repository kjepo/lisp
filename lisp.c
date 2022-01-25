#include <stdio.h>
#include <stdlib.h>
#include "hashtab.h"

#define NL printf("\n");

// Objects are tagged 32-bit values

typedef uint32_t Obj;

// Objects are tagged with 2 most significant bits
// number      00bbbbbb ... bbbbbbbb
// atom        01bbbbbb ... bbbbbbbb
// pair        10bbbbbb ... bbbbbbbb
// procedure   11bbbbbb ... bbbbbbbb

#define PAIR_TAG 0
#define ATOM_TAG 1
#define NUM_TAG  2
#define PROC_TAG 3

Obj mknum(int n) {
  return n | (NUM_TAG << 30);
}

Obj mkatom(char *id) {
  int n = lookup(id);
  return n | (ATOM_TAG << 30);
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


Obj NIL_ATOM, TRUE_ATOM, IF_ATOM, EQ_ATOM, LET_ATOM, ADD_ATOM, SUB_ATOM,
  MUL_ATOM, DIV_ATOM, DEFINE_ATOM, CAR_ATOM, CDR_ATOM, CONS_ATOM, ATOM_ATOM,
  QUOTE_ATOM, LETREC_ATOM, LAMBDA_ATOM;

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
Obj Stack[1024];
Obj val, unev, argl, proc;
Obj expr;

void init() {
  NIL_ATOM = mkatom("NIL"); TRUE_ATOM = mkatom("TRUE"); IF_ATOM = mkatom("IF");
  EQ_ATOM = mkatom("EQ");   LET_ATOM = mkatom("LET"); ADD_ATOM = mkatom("ADD");
  SUB_ATOM = mkatom("SUB"); MUL_ATOM = mkatom("MUL"); DIV_ATOM = mkatom("DIV");
  CAR_ATOM = mkatom("CAR"); CDR_ATOM = mkatom("CDR");
  CONS_ATOM = mkatom("CONS"); ATOM_ATOM = mkatom("ATOM");
  QUOTE_ATOM = mkatom("QUOTE"); LETREC_ATOM = mkatom("LETREC");
  DEFINE_ATOM = mkatom("DEFINE"); LAMBDA_ATOM = mkatom("LAMBDA");
  cont = EVAL_DISPATCH;
  val = NIL_ATOM;
  unev = NIL_ATOM;
  argl = NIL_ATOM;
  proc = NIL_ATOM;
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
  case ATOM_TAG:
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
int is_variable()        { return ATOM_TAG == objtype(expr); }
int is_quote()           { return QUOTE_ATOM == car(expr); }
int is_if()              { return 0; }
int is_assignment()      { return 0; }
int is_definition()      { return 0; }
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
	cont = EV_VARIABLE;
      } else if (is_quote()) {
	val = car(cdr(expr));
	continue;
      } else if (is_if()) {
	cont = EV_IF;
      } else if (is_assignment()) {
	cont = EV_ASSIGNMENT;
      } else if (is_definition()) {
	cont = EV_DEFINITION;
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
  init();
  eval_dispatch();
  expr = cons(QUOTE_ATOM, cons(mknum(17), 0));
  eval_dispatch();
}
