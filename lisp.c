/*
 * This doesn't work 
 *
 * (define id (lambda (x) x))
 * (id 3)
 *
 * Also, this changes global x
 *
 * (lambda (x) x) 3)
 *
 * For --load filename.scm 
 *
 * lisp should add ".scm" if it isn't provided 
 * after reading filename.scm it should continue reading from stdin
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <readline/readline.h>
#include "lisp.h"
#include "hashtab.h"

int verbose = 0;

void error(char *s)   { printf("%s\n", s); exit(1); }
void verify(int test) { assert(test); printf("OK!\n"); }

FILE *fp;

void display(Obj);
void display2(Obj, int);

// Objects are tagged with 3 most significant bits
// number      000bbbbbb ... bbbbbbbb
// symbol      001bbbbbb ... bbbbbbbb
// pair        010bbbbbb ... bbbbbbbb
// procedure   011bbbbbb ... bbbbbbbb
// primitive   100bbbbbb ... bbbbbbbb
// string      101
// boolean     110bbbbbb ... bbbbbbbb
// array       111

#define PAIR_TAG   0
#define SYMBOL_TAG 1
#define NUM_TAG    2
#define PROC_TAG   3
#define PRIM_TAG   4
#define STRING_TAG 5
#define BOOL_TAG   6
#define ARRAY_TAG  7

int objval(Obj n) {
  return n & (0x1fff);
}

int objtype(Obj n) {
  return (n >> 29) & 0x7;
}

void unbound(Obj id) {
  printf("Unbound variable %s\n", find(objval(id)));
}

#define MEMSIZE 2048

Obj thecars[MEMSIZE];
Obj thecdrs[MEMSIZE];
Obj free_index = 1;		       // can't start at 0 because 0 means NIL

Obj NIL = 0;

#define CAR(p) (thecars[p])
#define CDR(p) (thecdrs[p])
#define CDDR(p) (CDR(CDR(p)))
#define CDDDR(p) (CDR(CDR(CDR(p))))
#define CADR(p) (CAR(CDR(p)))
#define CADDR(p) (CAR(CDR(CDR(p))))
#define CADDDR(p) (CAR(CDR(CDR(CDR(p)))))

Obj mknum(int n) { return n | (NUM_TAG << 29); }
Obj mksym(char *id) { return lookup(id) | (SYMBOL_TAG << 29); }
Obj mkbool(char *id) { return lookup(id) | (BOOL_TAG << 29); }

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
  return (l == NIL ? m : cons(CAR(l), append(CDR(l), m)));
}

Obj adjoin_arg(Obj arg, Obj arglist) {
  return append(arglist, list(arg));
}

Obj mkproc(Obj parameters, Obj body, Obj env) {
  /* a procedure is a triple (parameters body env) */
  Obj p = cons(parameters, cons(body, cons(env, 0)));
  return objval(p) | (PROC_TAG << 29);
  //  return p;
}

typedef enum {
  PRIM_CAR,
  PRIM_CDR,
  PRIM_CONS,
  PRIM_PAIRP,
} Primitive;

Obj mkprim(Primitive p) {
  return p | (PRIM_TAG << 29);
}



Obj TRUE_SYM, IF_SYM, EQ_SYM, LET_SYM, ADD_SYM, SUB_SYM,
  MUL_SYM, DIV_SYM, DEFINE_SYM, CAR_SYM, CDR_SYM, CONS_SYM, ATOM_SYM,
  QUOTE_SYM, LETREC_SYM, LAMBDA_SYM, SETBANG_SYM;

typedef enum {
  PRINT_RESULT,			/* 0 */
  EV_IF_DECIDE,
  EV_IF_CONSEQUENT,
  EV_IF_ALTERNATIVE,
  EV_ASSIGNMENT_1,		
  EV_DEFINITION_1,		
  EV_APPL_DID_OPERATOR,		
  EV_APPL_ACCUMULATE_ARG,	
  EV_APPL_ACCUM_LAST_ARG, 	
  EV_SEQUENCE_CONTINUE,		
  EVAL_DISPATCH,		/* 10 */
  EV_SELF_EVAL,			
  EV_VARIABLE,			
  EV_QUOTED,			
  EV_IF,			
  EV_ASSIGNMENT,		
  EV_DEFINITION,		
  EV_LAMBDA,			
  EV_BEGIN,
  EV_APPLICATION,
  EV_APPL_OPERAND_LOOP,		/* 20 */
  EV_APPL_LAST_ARG,
  EV_SEQUENCE,
  EV_SEQUENCE_LAST_EXP,
  APPLY_DISPATCH,
  PRIMITIVE_APPLY,
  COMPOUND_APPLY,
  UNKNOWN_PROCEDURE_TYPE,  
  UNKNOWN_EXPRESSION_TYPE	/* 28 */
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
    return cons(cons(CAR(vars), CAR(vals)),
		pairup(CDR(vars), CDR(vals)));
    //    return cons(cons(CAR(vars), cons(CAR(vals), NIL)), pairup(CDR(vars), CDR(vals)));
}

Obj bind(Obj vars, Obj vals, Obj env) {
  return append(pairup(vars, vals), env);
}

Obj bind1(Obj var, Obj value, Obj env) {
  return cons(cons(var, value), env);
}

void prepend(Obj x, Obj l) {
  Obj first = CAR(l);
  Obj rest = CDR(l);
  CAR(l) = x;
  CDR(l) = cons(first, rest);
}

void add_binding(Obj var, Obj val, Obj env) {
  //  Obj v = cons(var, cons(val, 0));
  Obj v = cons(var, val);
  prepend(v, env);
}

/*
// env[var->value]
void add_binding(Obj var, Obj value, Obj env0) {
  env = bind(var, value, env0);
}
*/

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
    CDR(pair) = val;		/* overwrite old definition */
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


void init() {
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
  cont = EVAL_DISPATCH;
  val = NIL;
  unev = NIL;
  argl = NIL;
  proc = NIL;
  StackPtr = 0;
}

// problem is mk_proc creates a list and adds a tag which is not a pair that CAR/CDR works on

int is_pair(expr)        { return PAIR_TAG == objtype(expr) && expr != NIL; }
int is_self_evaluating() { return NUM_TAG == objtype(expr) || BOOL_TAG == objtype(expr); }
int is_variable()        { return SYMBOL_TAG == objtype(expr); }
int is_quote()           { return PAIR_TAG == objtype(expr) && QUOTE_SYM == CAR(expr); }
int is_if()              { return PAIR_TAG == objtype(expr) && IF_SYM == CAR(expr); }
int is_assignment()      { return PAIR_TAG == objtype(expr) && SETBANG_SYM == CAR(expr); }
int is_definition()      { return PAIR_TAG == objtype(expr) && DEFINE_SYM == CAR(expr); }
int is_lambda()          { return PAIR_TAG == objtype(expr) && LAMBDA_SYM == CAR(expr); }
int is_begin()           { return 0; }
int is_application()     { return PAIR_TAG == objtype(expr); }
int is_primitive(Obj p)  { return PRIM_TAG == objtype(p); }
int is_compound(Obj p)   { return PROC_TAG == objtype(p); }

/* argl is a list of arguments (a1 a2 a3 .. aN) */
/* if the primitive function only takes one argument, it is a1 */
/* if the primitive function takes two arguments, they are a1 and a2 */
void prim_car()   { val = CAR(CAR(argl)); }
void prim_cdr()   { val = CDR(CAR(argl)); }
void prim_cons()  { val = cons(CAR(argl), CADR(argl)); }
void prim_pairp() { val = is_pair(CAR(argl)) ? True : False; }

void (*primitives[])() = { prim_car, prim_cdr, prim_cons, prim_pairp };


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
      display_registers("EV_APPL_DID_OPERATOR [2]");      
      if (unev == NIL)		/* no operands */
	label = APPLY_DISPATCH;
      else {
	push(proc);
	label = EV_APPL_OPERAND_LOOP;
      }
      continue;

    case EV_APPL_OPERAND_LOOP:
      display_registers("EV_APPL_OPERAND_LOOP");
      push(argl);
      expr = CAR(unev);		/* 1st operand */
      if (!CDR(unev)) {		/* last operand */
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
      printf("proc: "); display(proc); NL; dump_memory(); NL;
      unev = CAR(objval(proc));	     /* procedure parameters */
      env = CADDR(objval(proc));     /* procedure environment */
      env = bind(unev, argl, env);
      unev = CADR(objval(proc));     /* procedure body */
      label = EV_SEQUENCE;
      continue;

    case EV_SEQUENCE:
      display_registers("EV_SEQUENCE");
      expr = CAR(unev);		/* first expression */
      if (!CDR(unev))		/* last expression */
	label = EV_SEQUENCE_LAST_EXP;
      else {
	push(unev);
	push(env);
	cont = EV_SEQUENCE_CONTINUE;
	label = EVAL_DISPATCH;
      }
      display_registers("EV_SEQUENCE [exit]");      
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
      unev = CDR(unev);
      label = EV_SEQUENCE;
      continue;

    case EV_APPL_ACCUMULATE_ARG:
      display_registers("EV_APPL_ACCUMULATE_ARG");
      unev = pop();
      env = pop();
      argl = pop();
      argl = adjoin_arg(val, argl);
      unev = CDR(unev);		/* rest operands */
      label = EV_APPL_OPERAND_LOOP;
      continue;

    case EV_APPL_ACCUM_LAST_ARG:
      display_registers("EV_APPL_ACCUMULATE_LAST_ARG");
      argl = pop();
      argl = adjoin_arg(val, argl);
      proc = pop();
      display_registers("EV_APPL_ACCUMULATE_LAST_ARG [exit]");
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
      val = CADR(expr);
      label = cont;
      continue;

    case EV_IF:
      display_registers("EV_IF");
      push(expr);		/* save expression for later */
      push(env);
      push(cont);
      cont = EV_IF_DECIDE;
      expr = CADR(expr);	/* if-predicate */
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
      expr = CADDR(expr);	/* if-consequent */
      label = EVAL_DISPATCH;
      continue;

    case EV_IF_ALTERNATIVE:
      display_registers("EV_IF_ALTERNATIVE");
      if (CDDDR(expr))
	expr = CADDDR(expr);
      else
	expr = NIL;
      label = EVAL_DISPATCH;
      continue;

    case EV_ASSIGNMENT:
      display_registers("EV_ASSIGNMENT");
      unev = CADR(expr);	/* assignment variable */
      push(unev);
      expr = CADDR(expr); /* assignment value */
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
      unev = CADR(expr);
      push(unev);
      expr = CADDR(expr);
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
      display_registers("EV_DEFINITION_1 [2]");      
      define_variable(unev, val, env);
      label = cont;
      continue;

    case EV_LAMBDA:
      display_registers("EV_LAMBDA");
      unev = CADR(expr); 	/* parameters */
      expr = CDDR(expr);	/* body */
      val = mkproc(unev, expr, env);
      label = cont;
      continue;

    case EV_BEGIN:
      display_registers("EV_BEGIN");
      error("NYI");

    case EV_APPLICATION:
      display_registers("EV_APPLICATION");
      push(cont);
      push(env);
      unev = CDR(expr);
      push(unev);
      expr = CAR(expr);
      cont = EV_APPL_DID_OPERATOR;
      label = EVAL_DISPATCH;
      continue;

    case PRINT_RESULT:
      printf("===> ");
      display(val); NL;
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

void setup_environment() {
  // an environment BINDING id -> value is represented simply as cons(id, value)
  // an ENVIRONMENT is a list of bindings ( ( id . value ) ( id . value ) .... )

  env = 0;
  env = bind1(mksym("#f"), False, env);
  env = bind1(mksym("#t"), True, env);
  env = bind1(mksym("x"), mknum(8), env);
  env = bind1(mksym("y"), mknum(7), env);
  env = bind1(mksym("car"), mkprim(PRIM_CAR), env);
  env = bind1(mksym("cdr"), mkprim(PRIM_CDR), env);
  env = bind1(mksym("cons"), mkprim(PRIM_CONS), env);
  env = bind1(mksym("pair?"), mkprim(PRIM_PAIRP), env);
}

void eval() {
  display(expr); NL;
  eval_dispatch();
}

// scanner and parser for LISP-style input
typedef enum toktype { END, ID, NUM, LPAR = '(', RPAR = ')', DOT = '.' } Token;
Token token;  // current token
char id[80];    // string value when token == ID
int nval;       // numeric value when token == NUM

int legal_symbol_start(char ch) { return isalpha(ch) || strchr("#+-.*/<=>!?:$%_&~^", ch); }   
int legal_symbol_rest(char ch) { return isalnum(ch) || strchr("#+-.*/<=>!?:$%_&~^", ch); }   

Token scan2() {
  char ch;
  do {                     // skip whitespace
    if (EOF == (ch = fgetc(fp)))
      return token = END;
  } while (isspace(ch));

  switch (ch) {
  case LPAR:
    return token = LPAR;
  case RPAR:
    return token = RPAR;
  case DOT:
    return token = DOT;
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    ungetc(ch, fp);
    fscanf(fp, "%d", &nval);
    return token = NUM;
  default:
    if (legal_symbol_start(ch) && ch != ' ') {
      char *p = id;
      *p++ = ch;
      while ((ch = fgetc(fp)) && legal_symbol_rest(ch))
	*p++ = ch;
      ungetc(ch, fp);
      *p = 0;
      return token = ID;
    } else
      printf("lexical error: %c\n", ch);
    break;
  }
  return token = END;
}

Token scan() {
  Token t = scan2();
  return t;
  printf("got token %d ", t);
  if (t == ID)
    printf("(%s)", id);
  if (t == NUM)
    printf("(%d)", nval);
  NL;
  return t;
}


void expect(Token tok, char *msg) {
  if (token == tok)
    scan();
  else
    error(msg);
}

Obj parse_atom() {
  Obj x;
  char *p;
  if (token == ID) {
    x = mksym(id);
  } else if (token == NUM)
    x = mknum(nval);
  else
    error("expected number or symbol.");
  scan();
  return x;
}

Obj parse();
Obj parse2();

Obj parse3(Obj p) {
  Obj x;
  if (token == RPAR) {
    x = cons(p, NIL);
    scan();
  } else if (token == DOT) {
    scan();
    x = cons(p, parse());
    expect(RPAR, "expected ).");
  } else if (token == NUM || token == ID || token == LPAR)
    x = cons(p, parse2());
  else
    printf("expected (, ), ., number, or symbol but got %d\n", token);
  return x;
}

Obj parse2() {
  return parse3(parse());
}

Obj parse() {			/* recursive-descent parser */
  if (token == ID || token == NUM)
    return parse_atom();
  else if (token == LPAR) {
    scan();
    return parse2();
  } else if (token == END)
    return 0;
  else
    error("expected number, symbol, or '('.");
  return 0;
}

void rep() {
  scan();
  while ((expr = parse()))
    eval();
}

int main(int argc, char *argv[]) {
  fp = stdin;

  init();
  setup_environment();

  for (int i = 1; i < argc; i++) {
    if (argv[i][0] == '-') {
      if (!strncmp(argv[i], "--load", 6)) {
	if (!(fp = fopen(argv[i+1], "r"))) {
	  fprintf(stderr, "%s: could not open %s\n", argv[0], argv[i+1]);
	  exit(1);
	} else {
	  printf("reading from %s\n", argv[i+1]);
	  rep();
	  fp = stdin;
	}
      } else if (argv[i][1] == 'v') {
	verbose = 1;
      } else {
	fprintf(stderr, "usage: %s [-v] [--load filename]\n", argv[0]);
	exit(1);
      }
    }
  }

  /*
  printf("testing readline:\n");
  char* input = readline("prompt> ");
  printf("%s\n", input);
  */


  rep();

  if (verbose)
    dump_memory();
  printf("env: "); display(env); NL;
  if (verbose)
    dump_hashtab();

}
