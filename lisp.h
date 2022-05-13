#ifndef LISPH
#define LISPH

#include <unistd.h>

#define NL printf("\n");
#define MEMSIZE (2<<25)
#define STACKSIZE 100

typedef union {
  double as_double;
  uint64_t as_int;
} Obj;

#define OUR_NAN(v)     ((v.as_int & 0x7ffc000000000000) == 0x7ffc000000000000)
#define NAN_MASK       0xffff000000000000  /* 1 11111111111 1111 address */
#define NAN_VALUE(v)   (v.as_int & (~NAN_MASK))
#define EQ(v1, v2)     ((v1).as_int == (v2).as_int)

// Symbols
#define SYMBOL_MASK    0x7ffc000000000000  /* 0 11111111111 1100 address */
#define IS_SYMBOL(v)   ((v.as_int & NAN_MASK) == SYMBOL_MASK)
#define MAKE_SYMBOL(p) { .as_int = (uint64_t) (p) | SYMBOL_MASK };

// Primitives
#define PRIM_MASK      0x7ffd000000000000  /* 0 11111111111 1101 address */
#define IS_PRIM(v)     ((v.as_int & NAN_MASK) == PRIM_MASK)
#define MAKE_PRIM(p)   { .as_int = (uint64_t) (p) | PRIM_MASK };

// Strings
#define STR_MASK       0x7fff000000000000  /* 0 11111111111 1111 address */
#define IS_STR(v)      ((v.as_int & NAN_MASK) == STR_MASK)
#define MAKE_STR(p)    { .as_int = (uint64_t) (p) | STR_MASK };

// S-expressions
#define SEXPR_MASK     0xfffc000000000000  /* 1 11111111111 1100 address */
#define IS_SEXPR(v)    ((v.as_int & NAN_MASK) == SEXPR_MASK)
#define MAKE_SEXPR(p)  { .as_int = (uint64_t) (p) | SEXPR_MASK };

// Arrays
#define ARRAY_MASK     0xfffe000000000000  /* 1 11111111111 1110 address */
#define IS_ARRAY(v)    ((v.as_int & NAN_MASK) == ARRAY_MASK)
#define MAKE_ARRAY(p)  { .as_int = (uint64_t) (p) | ARRAY_MASK };

// Broken heart
#define BROKEN_MASK    0xffff000000000000  /* 1 11111111111 1111 address */
#define IS_BROKEN(v)   ((v.as_int & NAN_MASK) == BROKEN_MASK)
#define MAKE_BROKEN(p) { .as_int = (uint64_t) (p) | BROKEN_MASK };

// Doubles
#define IS_DOUBLE(v)   (!OUR_NAN(v))
#define MAKE_DOUBLE(d) { .as_double = d };
#define DOUBLEVALUE(v) (v.as_double)

extern int objtype(Obj);
extern int objval(Obj);
extern int is_pair(Obj);
extern int is_compound(Obj);
extern Obj mkpointer(int);
extern int is_nil(Obj);
Obj *thecars, *thecdrs, *newcars, *newcdrs;
extern int free_index;
extern Obj NIL, True, False, env, val, unev, argl, proc;
extern Obj expr, prim_proc, stack, conscell, tmp1, tmp2, tmp3;

extern int verbose, lineno;

#define car(v) (thecars[NAN_VALUE(v)])
#define cdr(v) (thecdrs[NAN_VALUE(v)])

#define cddr(p) (cdr(cdr(p)))
#define caadr(p) (car(car(cdr(p))))
#define cdddr(p) (cdr(cdr(cdr(p))))
#define cdadr(p) (cdr(car(cdr(p))))
#define cadr(p) (car(cdr(p)))
#define caddr(p) (car(cdr(cdr(p))))
#define cadddr(p) (car(cdr(cdr(cdr(p)))))

typedef enum { PRINT_RESULT, EV_IF_DECIDE, EV_IF_CONSEQUENT, EV_IF_ALTERNATIVE,
  EV_ASSIGNMENT_1, EV_DEFINITION_1, EV_APPL_DID_OPERATOR,
  EV_APPL_ACCUMULATE_ARG, EV_APPL_ACCUM_LAST_ARG, EV_SEQUENCE_CONTINUE,
  EVAL_DISPATCH, EV_SELF_EVAL, EV_VARIABLE, EV_QUOTED, EV_IF, EV_ASSIGNMENT,
  EV_DEFINITION, EV_LAMBDA, EV_NLAMBDA, EV_BEGIN, EV_APPLICATION, EV_APPL_OPERAND_LOOP,
  EV_APPL_LAST_ARG, EV_SEQUENCE, EV_SEQUENCE_LAST_EXP, APPLY_DISPATCH,
  PRIMITIVE_APPLY, COMPOUND_APPLY, MACRO_APPLY, UNKNOWN_PROCEDURE_TYPE,
  UNKNOWN_EXPRESSION_TYPE } Continuation;

extern Obj cont;
extern Continuation label;

extern char *continuation_string[];

Obj IF_SYM, EQ_SYM, ADD_SYM, SUB_SYM, MUL_SYM, DIV_SYM, DEFINE_SYM, CAR_SYM,
  CDR_SYM, CONS_SYM, QUOTE_SYM, LAMBDA_SYM, NLAMBDA_SYM, SETBANG_SYM, BEGIN_SYM,
  PROCEDURE_SYM, MACRO_SYM;

#endif
