#ifndef LISPH
#define LISPH

#define NL printf("\n");
#define MEMSIZE (2<<12)
#define STACKSIZE 100

// Objects are tagged 32-bit values

typedef uint32_t Obj;

// tagged pointers - you should let PAIR_TAG = 0 or all bets are off
#define PAIR_TAG   0
#define SYMBOL_TAG 1
#define NUM_TAG    2
#define PRIM_TAG   3
#define STR_TAG    4
#define BOOL_TAG   5
#define ARRAY_TAG  6
#define BROKEN_TAG 7		/* broken heart tag for garbage collector */

extern int objtype(Obj);
extern int objval(Obj);
extern int is_pair(Obj);
extern int is_compound(Obj);
extern Obj mkpointer(int);

Obj *thecars, *thecdrs, *newcars, *newcdrs;
extern Obj NIL, free_index, True, False, env, val, unev, argl, proc, expr, prim_proc, stack, conscell, tmp1, tmp2, tmp3;

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

extern Continuation cont;

extern char *continuation_string[];

Obj IF_SYM, EQ_SYM, LET_SYM, ADD_SYM, SUB_SYM, MUL_SYM, DIV_SYM, DEFINE_SYM, CAR_SYM, CDR_SYM,
  CONS_SYM, ATOM_SYM, QUOTE_SYM, LAMBDA_SYM, SETBANG_SYM, BEGIN_SYM, PROCEDURE_SYM;

#endif
