 /**
  * A simple explicit-control, garbage-collecting    _    _           
  * register-machine Scheme-like Lisp interpreter   | |  (_)____ __   
  * written in 2022 by Kjell Post, but based on a   | |__| (_-< '_ \
  * dormant project from a college course in 1986   |____|_/__/ .__/  
  *                                                           |_|     
 **/

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <signal.h>
#include <unistd.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <assert.h>
#include "lisp.h"
#include "print.h"
#include "hashtab.h"
#include "gc.h"

Obj NIL=0, free_index=1, True, False, env, val, unev, argl, proc, expr, stack;
Obj conscell, tmp1, tmp2, tmp3, prim_proc;
Continuation cont, label;

int verbose = 0;
char *fname;
FILE *fp;
int lineno;
char *progname;
char *input;
static jmp_buf jmpbuf;

void error(char *s)   {
  printf("%s on line %d, file %s\n", s, (lineno-1), fname);
  longjmp(jmpbuf, 1);
}

void INThandler(int sig) {
  char c;
  signal(sig, SIG_IGN);
  printf("Interrupt\n");
  signal(SIGINT, INThandler);
  longjmp(jmpbuf, 1);
}

int objtype(Obj n) { return n & 7; }
int objval(Obj n)  {
  if (objtype(n) == NUM_TAG && (n & 0x80000000)) /* handle negative number */
      return ((n >> 3) | 0xe0000000);
  return n >> 3;
}

void unbound(Obj id) {
  printf("Unbound variable %s at line %d, file %s\n",
	 find(objval(id)), lineno, fname);
  longjmp(jmpbuf, 1);
}

Obj mkpointer(int p) { return ((p) << 3) | PAIR_TAG; }

Obj cons(Obj car_, Obj cdr_) {
  thecars[free_index] = car_;
  thecdrs[free_index] = cdr_;
  conscell = mkpointer(free_index);
  if (++free_index >= MEMSIZE && gc()) {
    fprintf(stderr, "Sorry - memory is full, even after GC\n");
    exit(1);
  }
  return conscell;
}

typedef enum {
  PRIM_CAR, PRIM_CDR, PRIM_CONS, PRIM_PAIRP, PRIM_PLUS, PRIM_MINUS, PRIM_TIMES,
  PRIM_DIV, PRIM_EQ, PRIM_EQP, PRIM_LT,  PRIM_GT, PRIM_DISPLAY, PRIM_NUMBERP,
  PRIM_SYMBOLP, PRIM_NULLP, PRIM_EXIT, PRIM_FILE, PRIM_EVAL, PRIM_RND } Primitive;

Obj mknum(int n) { return (n << 3) | NUM_TAG; }
Obj mkstr(char *str) { return ((lookup(str)) << 3) | STR_TAG; }
Obj mksym(char *id) { return ((lookup(id)) << 3) | SYMBOL_TAG; }
Obj mkbool(char *id) { return ((lookup(id)) << 3) | BOOL_TAG; }
Obj mkprim(Primitive p) { return (p << 3) | PRIM_TAG; }

void push(Obj x) { stack = cons(x, stack); }

Obj pop() {
  if (stack == NIL) {
    error("Stack underflow!");
    return NIL; 		/* not reached */
  } else {
    Obj x = car(stack);
    stack = cdr(stack);
    return x;
  }
}

void gc_need(int n) {
  if (free_index + n >= MEMSIZE)
    gc();
  if (free_index + n >= MEMSIZE)
    error("Out of memory");
}

// a procedure is a triple (parameters body env)
Obj mkproc(Obj parameters, Obj body, Obj env) { 
  tmp1 = env;  tmp2 = body;  tmp3 = parameters;
  gc_need(5);
  parameters = tmp3; body = tmp2; env = tmp1;
  return cons(PROCEDURE_SYM, cons(parameters, cons(body, cons(env, NIL))));
}

// destructive append, sets the cdr of l to m (unless l is NIL of course)
Obj concat(Obj l, Obj m) {   
  Obj head = l;
  if (l == NIL)
    return m;
  while (cdr(l) != NIL)
    l = cdr(l);
  cdr(l) = m;
  return head;
}

Obj adjoin_arg(Obj arg, Obj arglist) {
  tmp1 = arg; tmp2 = arglist;
  gc_need(2);
  arg = tmp1; arglist = tmp2;
  return concat(arglist, cons(arg, NIL));
}

int length(Obj p) {
  int n = 0;
  while (p != NIL) {
    p = cdr(p);
    n++;
  }
  return n;
}

Obj pairup_aux(Obj vars, Obj vals) {
  if (vars == NIL)
    return NIL;
  else
    return cons(cons(car(vars), car(vals)), pairup_aux(cdr(vars), cdr(vals)));
}

Obj pairup(Obj vars, Obj vals) {
  if (vars == NIL)
    return NIL;
  int n = length(vars);
  int m = length(vals);
  if (m != n) {
    display(vars); printf(" "); display(vals); NL;
    error("Error: length mismatch between parameters and arguments");
    return NIL; 		// not reached
  } else {
    tmp1 = vars;
    tmp2 = vals;
    gc_need(2*n);
    return pairup_aux(tmp1, tmp2);
  }
}

Obj bind(Obj vars, Obj vals, Obj environment) {
  if (objtype(vars) != PAIR_TAG) { // ((lambda l body) '(1 2 3)) => l/'(1 2 3)
    tmp3 = environment;
    tmp1 = cons(vars, NIL);
    tmp2 = cons(vals, NIL);
    tmp2 = pairup(tmp1, tmp2);
    return concat(tmp2, tmp3);
  } else {   // ((lambda (x y z) body) '(1 2 3)) => bind x/1, y/2, z/3
    tmp3 = environment;
    tmp1 = pairup(vars, vals);	// <--- uses tmp1 and tmp2
    environment = tmp3;
    return concat(tmp1, environment);
  }
}

void prepend(Obj x, Obj l) {
  Obj first = car(l);
  Obj rest = cdr(l);
  tmp1 = x; tmp2 = l;
  conscell = cons(first, rest);
  x = tmp1; l = tmp2;
  car(l) = x;
  cdr(l) = conscell;
}

void add_binding(Obj var, Obj val, Obj env) {
  tmp1 = env;
  tmp2 = cons(var, val);
  prepend(tmp2, tmp1);
}

// find the first pair in env whose car equals var and return that pair
Obj assoc(Obj var, Obj env) {
  if (env == NIL)
    return NIL;
  else if (car(car(env)) == var)
    return car(env);
  else
    return assoc(var, cdr(env));
}

Obj env_lookup(Obj var, Obj env) {      // env[var]
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
    cdr(pair) = val;            // overwrite old definition
  else
    add_binding(var, val, env);
}

void init_symbols() {
  False = mkbool("#f");  True = mkbool("#t");
  IF_SYM = mksym("if");  EQ_SYM = mksym("eq");  ADD_SYM = mksym("add");
  SUB_SYM = mksym("sub");  MUL_SYM = mksym("mul");  DIV_SYM = mksym("div");
  CAR_SYM = mksym("car");  CDR_SYM = mksym("cdr");  CONS_SYM = mksym("cons");
  QUOTE_SYM = mksym("quote"); DEFINE_SYM = mksym("define");
  LAMBDA_SYM = mksym("lambda"); SETBANG_SYM = mksym("set!");
  BEGIN_SYM = mksym("begin"); PROCEDURE_SYM = mksym("procedure");
  val = unev = argl = proc = stack = NIL;
  cont = mknum(EVAL_DISPATCH);
  srandom(getpid());
}

void init_env() {
  // an environment BINDING id -> value is represented as cons(id, value)
  // an ENVIRONMENT is a list of bindings ( ( id . value ) ( id . value ) ... )
  prim_proc = cons(NIL, NIL);  // can't be NIL (make space for 1 binding)
  add_binding(mksym("cons"), mkprim(PRIM_CONS), prim_proc);
  add_binding(mksym("display"), mkprim(PRIM_DISPLAY), prim_proc);
  add_binding(mksym("#f"), False, prim_proc);
  add_binding(mksym("#t"), True, prim_proc);
  add_binding(mksym("car"), mkprim(PRIM_CAR), prim_proc);
  add_binding(mksym("cdr"), mkprim(PRIM_CDR), prim_proc);
  add_binding(mksym("pair?"), mkprim(PRIM_PAIRP), prim_proc);
  add_binding(mksym("plus"), mkprim(PRIM_PLUS), prim_proc);
  add_binding(mksym("minus"), mkprim(PRIM_MINUS), prim_proc);
  add_binding(mksym("times"), mkprim(PRIM_TIMES), prim_proc);
  add_binding(mksym("div"), mkprim(PRIM_DIV), prim_proc);
  add_binding(mksym("=="), mkprim(PRIM_EQ), prim_proc);  // ??
  add_binding(mksym("eq?"), mkprim(PRIM_EQP), prim_proc);
  add_binding(mksym("<"), mkprim(PRIM_LT), prim_proc);
  add_binding(mksym(">"), mkprim(PRIM_GT), prim_proc);
  add_binding(mksym("number?"), mkprim(PRIM_NUMBERP), prim_proc);
  add_binding(mksym("symbol?"), mkprim(PRIM_SYMBOLP), prim_proc);
  add_binding(mksym("null?"), mkprim(PRIM_NULLP), prim_proc);
  add_binding(mksym("exit"), mkprim(PRIM_EXIT), prim_proc);
  add_binding(mksym("file"), mkprim(PRIM_FILE), prim_proc);
  add_binding(mksym("eval"), mkprim(PRIM_EVAL), prim_proc);
  add_binding(mksym("rnd"), mkprim(PRIM_RND), prim_proc);
  add_binding(mksym("current-environment"), prim_proc, prim_proc);
}

int is_num(Obj p)        { return NUM_TAG == objtype(p); }
int is_str(Obj p)        { return STR_TAG == objtype(p); }
int is_bool(Obj p)       { return BOOL_TAG == objtype(p); }
int is_variable()        { return SYMBOL_TAG == objtype(expr); }
int is_primitive(Obj p)  { return PRIM_TAG == objtype(p); }
int is_compound(Obj p)   { return is_pair(p) && PROCEDURE_SYM == car(p); }
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

void checklist(Obj p, char *fcnname) {
  if (is_pair(p))
    return;
  printf("The object "); display(p);
  printf(" passed as the first argument of %s, is not of the correct type.\n", fcnname);
  printf("(Line %d, file %s)\n", lineno, fname);
  longjmp(jmpbuf, 1);
}

int checknr(Obj p, char *opname) {
  if (is_num(p))
    return objval(p);
  printf("The object "); display(p);
  printf(" passed as an argument of %s, is not of the correct type.\n", opname);
  longjmp(jmpbuf, 1);
}

// argl is a list of arguments (a1 a2 a3 .. aN)
// if the primitive function only takes one argument, it is a1; if it's two then it's a1 and a2
void prim_plus() { val = mknum(checknr(car(argl), "plus") + checknr(cadr(argl), "plus")); }
void prim_minus() { val = mknum(checknr(car(argl), "minus") - checknr(cadr(argl), "minus")); }
void prim_times() { val = mknum(checknr(car(argl), "times") * checknr(cadr(argl), "times")); }
void prim_div() { val = mknum(checknr(car(argl), "times") / checknr(cadr(argl), "div")); }
void prim_eq() { val = checknr(car(argl), "=") == checknr(cadr(argl), "=") ? True : False; }
void prim_lt() { val = checknr(car(argl), "<") < checknr(cadr(argl), "<") ? True : False; }
void prim_gt() { val = checknr(car(argl), ">") > checknr(cadr(argl), ">") ? True : False; }
void prim_eqp() { val = (objval(car(argl)) == objval(cadr(argl))) ? True : False; }
void prim_car() { checklist(car(argl), "car"); val = car(car(argl)); }
void prim_cdr() { checklist(car(argl), "cdr"); val = cdr(car(argl)); }
void prim_cons() { val = cons(car(argl), cadr(argl)); }
void prim_pairp() { val = is_pair(car(argl)) ? True : False; }
void prim_nullp() { val = (car(argl) == NIL ? True : False); }
void prim_display() { display(car(argl)); }
void prim_numberp() { val = (objtype(car(argl)) == NUM_TAG ? True : False); }
void prim_symbolp() { val = (objtype(car(argl)) == SYMBOL_TAG ? True : False); }
void prim_exit() { longjmp(jmpbuf, 1); } 
void prim_file() { val = cons(mknum(lineno-1), cons(mkstr(fname), NIL)); }
void eval();
void prim_rnd() { val = mknum(random() & 0xffff); } 
void prim_eval() {		/* (eval '(plus 1 2) current-environment */
  push(expr); push(env);
  expr = car(argl);
  if (cadr(argl) != NIL)
    env = cadr(argl);
  eval();
  env = pop(); expr = pop();
}

void (*primitives[])() = {
  prim_car, prim_cdr, prim_cons, prim_pairp, prim_plus, prim_minus, prim_times,
  prim_div, prim_eq, prim_eqp, prim_lt, prim_gt, prim_display, prim_numberp,
  prim_symbolp, prim_nullp, prim_exit, prim_file, prim_eval, prim_rnd };

void eval() {			// evaluate expr in env
  label = EVAL_DISPATCH;
  cont = mknum(PRINT_RESULT);
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
        cont = mknum(EV_APPL_ACCUMULATE_ARG);
        label = EVAL_DISPATCH;
      }
      continue;

    case EV_APPL_LAST_ARG:
      display_registers("EV_APPL_LAST_ARG");
      cont = mknum(EV_APPL_ACCUM_LAST_ARG);
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
      label = objval(cont);
      continue;

    case COMPOUND_APPLY:
      display_registers("COMPOUND_APPLY");
      unev = cadr(proc);      /* procedure parameters */
      env = cadddr(proc);     /* procedure environment */
      env = bind(unev, argl, env);   /* bind formals to arguments */
      unev = caddr(proc);     /* procedure body */
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
        cont = mknum(EV_SEQUENCE_CONTINUE);
        label = EVAL_DISPATCH;
      }
      continue;

    case EV_SEQUENCE_LAST_EXP:
      display_registers("EV_SEQUENCE_LAST_EXP");
      cont = pop();
      label = EVAL_DISPATCH;
      continue;

    case UNKNOWN_PROCEDURE_TYPE:
      display(proc); NL;
      error("unknown procedure");
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
      display_registers("EV_APPL_ACCUMULATE_LAST_ARG [2]");
      continue;

    case EV_SELF_EVAL:
      display_registers("EV_SELF_EVAL");
      val = expr;
      label = objval(cont);
      continue;

    case EV_VARIABLE:
      display_registers("EV_VARIABLE");
      val = env_lookup(expr, env);
      label = objval(cont);
      continue;

    case EV_QUOTED:
      display_registers("EV_QUOTED");
      val = cadr(expr);
      label = objval(cont);
      continue;

    case EV_IF:
      display_registers("EV_IF");
      push(expr);               // save expression for later
      push(env);
      push(cont);
      cont = mknum(EV_IF_DECIDE);
      expr = cadr(expr);        // if-predicate
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
      unev = cadr(expr);        // assignment variable
      push(unev);
      expr = caddr(expr);	// assignment value
      push(env);
      push(cont);
      cont = mknum(EV_ASSIGNMENT_1);
      label = EVAL_DISPATCH;
      continue;

    case EV_ASSIGNMENT_1:
      display_registers("EV_ASSIGNMENT_1");
      cont = pop();
      env = pop();
      unev = pop();
      set_variable_value(unev, val, env);
      label = objval(cont);
      continue;

    case EV_DEFINITION:
      display_registers("EV_DEFINITION");
      unev = cadr(expr);
      push(unev);
      expr = caddr(expr);
      push(env);
      push(cont);
      cont = mknum(EV_DEFINITION_1);
      label = EVAL_DISPATCH;
      continue;

    case EV_DEFINITION_1:
      display_registers("EV_DEFINITION_1");
      cont = pop();
      env = pop();
      unev = pop();
      define_variable(unev, val, env);
      label = objval(cont);
      continue;

    case EV_LAMBDA:
      display_registers("EV_LAMBDA");
      unev = cadr(expr);        /* parameters */
      expr = cddr(expr);        /* body */
      val = mkproc(unev, expr, env);
      label = objval(cont);
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
      cont = mknum(EV_APPL_DID_OPERATOR);
      label = EVAL_DISPATCH;
      continue;

    case PRINT_RESULT:
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

// scanner and parser for LISP-style input
typedef enum toktype {
  END=1, ID, NUM, STR, LPAR = '(', RPAR = ')', DOT = '.', TICK = '\'' } Token;

Token token;                    // current token
char id[80];                    // string value when token == ID
int nval;                       // numeric value when token == NUM

int legal_symbol_start(char ch) { return isalpha(ch) || strchr("#+-.*/<=>!?:$%_&~^", ch); }
int legal_symbol_rest(char ch)  { return isalnum(ch) || strchr("#+-.*/<=>!?:$%_&~^", ch); }

void cmd(char *line) {
  if (0 == strcmp(line, ":quit"))
    exit(0);
  else if (0 == strcmp(line, ":help")) {
    printf(":quit ==> quit (ctrl-D works too)\n");
    printf(":help ==> this help text\n");
    printf(":env  ==> print environment\n");
    printf(":word ==> print word size\n");
    printf(":mem  ==> show free memory\n");
    printf(":gc   ==> force garbage collection\n");
  } else if (0 == strcmp(line, ":env")) {
    display(env); NL;
  } else if (0 == strcmp(line, ":word")) {
    printf("%lu\n", sizeof(Obj));    
  } else if (0 == strcmp(line, ":mem")) {
    printf("%d/%d cells used\n", free_index, MEMSIZE);
  } else if (0 == strcmp(line, ":gc")) {
    gc();
  } else
    printf("unknown command: %s\n", line);
}

void cr_readline() {
  char prompt[20], *p;
  size_t len;
  sprintf(prompt, "[%d] ", lineno++);
  if (fp != stdin) {
    input = NULL;
    if (getline(&input, &len, fp) == -1) // POSIX (but see README for options)
      input = "(exit)";
    return;
  }
  p = readline(prompt);
  if (!p)
    exit(0);
  add_history(p);
  if (p[0] == ':') {
    cmd(p);
    return cr_readline();
  }
  int n = strlen(p);
  input = malloc(n + 1);
  strcpy(input, p);		/* fixme: memory leak */
  input[n] = '\n';		/* add \n at end of input */
  input[n+1] = 0;
}

char nextchar() {
  while (input == 0 || *input == 0 || *input == ';')
    cr_readline();	
  return *input++;
}

char nextrealchar() {
  char ch; 
  do {
    ch = nextchar();
  } while (ch == ' ' || ch == '\t');
  return ch;
}

Token scan() {
  char *p, ch = nextrealchar(), lastchar;
  switch (ch) {
  case '\n':
    return token = END;
  case '\"':
    p = id;
    lastchar = ch;
    while ((ch = nextchar()) != EOF) {
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
        return token = STR;
      } else {
        *p++ = ch;
      }
      lastchar = ch;
    }
    error("unterminated string");
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
    p = id;
    do {
      *p++ = ch;
      ch = nextchar();
    } while (isdigit(ch));
    *p = 0;
    input--;
    sscanf(id, "%d", &nval);
    return token = NUM;
  default:
    if (legal_symbol_start(ch) && ch != ' ') {
      char cc = 0, *p = id;
      *p++ = ch;
      while ((ch = nextchar()) && legal_symbol_rest(ch))
        *p++ = ch;
      input--;
      *p = 0;
      // a number is an optional + or - followed by at least one or more digits
      // these are numbers: 1, -1, +1, -12 but these are not -, -1x, +, ++
      if (sscanf(id, "%d%c", &nval, &cc) == 1 && cc == 0)
        return token = NUM;
      return token = ID;
    } else
      return scan();
  }
}

Obj parse_atom() {
  if (token == ID) {
    return mksym(id);
  } else if (token == NUM) {
    return mknum(nval);
  } else if (token == STR) {
    return mkstr(id);
  } else {
    error("expected number or symbol.");
    return -1;			/* unreachable */
  }
}

Obj parse();
Obj parse_seq() {
  if (token == END) {		/* continue parsing past end-of-line */
    scan();
    return parse_seq();
  } else if (token == RPAR) {	/* end of list */
    return NIL;
  } else if (token == DOT) {	/* dotted pair, e.g., (1 2 . 3) */
    scan();
    Obj x = parse_atom();
    scan();
    if (token != ')')
      error("')' expected");
    return x;
  } else {			/* regular list, e.g., (1 2 3) */
    Obj x = parse();
    scan();
    return cons(x, parse_seq());
  }
}

Obj parse() {
  if (token == ID || token == NUM || token == STR) {
    return parse_atom();
  } else if (token == TICK) {
    scan();
    return cons(QUOTE_SYM, cons(parse(), NIL));
  } else if (token == LPAR) {
    scan();
    if (token == RPAR)
      return NIL;
    else
      return parse_seq();
  } else {
    error("expected number, symbol, or '('.");
    return -1;			/* never executed */
  }
}

void repl() {
  input = 0;
  lineno = 1;
  for (;;) {
    scan();
    if (strncmp(input, "exit)", 5) == 0 && fp != stdin)
      return;  /* kludge to stop reading from file */
    if (token == END)		/* reached trailing \n */
      continue;
    expr = parse();	      // printf("expr = "); display(expr); NL;
    env = prim_proc;
    eval();
    if (fp == stdin || verbose == -1) {
      printf(";Value: ");
      display(val); NL;
    }
  }
}

void slurp(char *f) {
  fname = f;
  if (!(fp = fopen(fname, "r"))) {
    fprintf(stderr, "%s: could not open %s\n", progname, fname);
    exit(1);
  } else {
    printf("reading %s...\n", fname);
    repl();
    fclose(fp);
    fp = stdin;
  }
}

int main(int argc, char *argv[]) {
  char *usage = "usage: %s [-q] [-h] [-v] [filename ...]\n";
  progname = argv[0];
  int libloaded = 0;
  printf("Welcome to Lisp, type \":help\" for help.\n");

  thecars = (Obj *)calloc(MEMSIZE, sizeof(Obj));
  thecdrs = (Obj *)calloc(MEMSIZE, sizeof(Obj));
  newcars = (Obj *)calloc(MEMSIZE, sizeof(Obj));
  newcdrs = (Obj *)calloc(MEMSIZE, sizeof(Obj));

  init_symbols();
  init_env();
  fp = stdin;
  fname = "stdin";
  signal(SIGINT, INThandler);
  if (setjmp(jmpbuf) == 1)
    goto repl;

  for (int i = 1; i < argc; i++) {
    if (argv[i][0] == '-') {
      if (argv[i][1] == 'q') {
	libloaded = 1;
      } else if (argv[i][1] == 'v') {
        verbose = 1;
      } else if (argv[i][1] == 's') {
        verbose = -1;
      } else if (argv[i][1] == 'h') {
        printf(usage, progname);
	printf("  _    _           This Lisp interpreter accepts the following commands:\n");
	printf(" | |  (_)____ __     -q  dont' load lib.scm\n");
	printf(" | |__| (_-< '_ \\    -h  prints this help message\n");
	printf(" |____|_/__/ .__/    -v  verbose mode (prints registers, memory, symbols)\n");
	printf("           |_|     The source for this project is at github.com/kjepo/lisp\n");
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
      fname = "stdin";
    }
  }

  if (!libloaded) {
    slurp("lib.scm");
    libloaded = 1;
  }

  lineno = 1;
 repl:
  fp = stdin;
  fname = "stdin";
  repl();

  if (verbose) {
    dump_memory();
    printf("env: "); display(env); NL;
    dump_hashtab();
  }
}
