#ifndef LISPH
#define LISPH

#define NL printf("\n");
#define MEMSIZE 8192
//#define MEMSIZE 105
#define STACKSIZE 100

// Objects are tagged 32-bit values

typedef uint32_t Obj;

// tagged pointers, you should let PAIR_TAG = 0 or all bets are off
#define PAIR_TAG   0
#define SYMBOL_TAG 1
#define NUM_TAG    2
#define BROKEN_TAG 3
#define PRIM_TAG   4
#define STR_TAG    5
#define BOOL_TAG   6
#define ARRAY_TAG  7

// use the PRIM_TAG with the highest number for broken heart
#define BROKENHEART 0xfffffff4

extern int objtype(Obj);
extern int objval(Obj);
extern int is_pair(Obj);
extern int is_compound(Obj);
extern Obj mkpair(int);
extern void update_rootset();
extern void restore_rootset();

Obj *thecars, *thecdrs, *newcars, *newcdrs;
extern Obj NIL, free_index, True, False, env, val, unev, argl, proc, expr, root, prim_proc, stack, conscell, tmp1, tmp2, tmp3;

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

Obj TRUE_SYM, IF_SYM, EQ_SYM, LET_SYM, ADD_SYM, SUB_SYM, MUL_SYM, DIV_SYM, DEFINE_SYM, CAR_SYM, CDR_SYM,
  CONS_SYM, ATOM_SYM, QUOTE_SYM, LETREC_SYM, LAMBDA_SYM, SETBANG_SYM, BEGIN_SYM, PROCEDURE_SYM;

//Obj Stack[STACKSIZE];
//int StackPtr;
#endif
