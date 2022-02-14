#ifndef LISPH
#define LISPH

#define NL printf("\n");
#define MEMSIZE 16384
#define STACKSIZE 100

// Objects are tagged 32-bit values

typedef uint32_t Obj;

// tagged pointers, you should let PAIR_TAG = 0 or all bets are off
#define PAIR_TAG   0
#define SYMBOL_TAG 1
#define NUM_TAG    2
#define PROC_TAG   3
#define PRIM_TAG   4
#define STR_TAG    5
#define BOOL_TAG   6
#define ARRAY_TAG  7

int objtype(Obj n);
int objval(Obj n);

Obj *thecars, *thecdrs, *newcars, *newcdrs;
Obj True, False;
Obj env, val, unev, argl, proc, expr;
extern Obj NIL;

extern int verbose;

#define car(p) (thecars[p>>3])
#define cdr(p) (thecdrs[p>>3])
#define cddr(p) (cdr(cdr(p)))
#define cdddr(p) (cdr(cdr(cdr(p))))
#define cadr(p) (car(cdr(p)))
#define caddr(p) (car(cdr(cdr(p))))
#define cadddr(p) (car(cdr(cdr(cdr(p)))))

typedef enum {  PRINT_RESULT, EV_IF_DECIDE, EV_IF_CONSEQUENT, EV_IF_ALTERNATIVE, EV_ASSIGNMENT_1,              
  EV_DEFINITION_1, EV_APPL_DID_OPERATOR, EV_APPL_ACCUMULATE_ARG, EV_APPL_ACCUM_LAST_ARG, EV_SEQUENCE_CONTINUE,
  EVAL_DISPATCH, EV_SELF_EVAL, EV_VARIABLE, EV_QUOTED, EV_IF, EV_ASSIGNMENT, EV_DEFINITION, EV_LAMBDA, EV_BEGIN,
  EV_APPLICATION, EV_APPL_OPERAND_LOOP, EV_APPL_LAST_ARG, EV_SEQUENCE, EV_SEQUENCE_LAST_EXP, APPLY_DISPATCH,
  PRIMITIVE_APPLY, COMPOUND_APPLY, UNKNOWN_PROCEDURE_TYPE, UNKNOWN_EXPRESSION_TYPE } Continuation;

Continuation cont;

extern char *continuation_string[];

Obj Stack[STACKSIZE];
int StackPtr;

#endif
