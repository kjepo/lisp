#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <math.h>
#include <limits.h>
#include <string.h>
#include "lisp.h"
#include "hashtab.h"
#include "print.h"

char *continuation_string[] = { "PRINT_RESULT", "EV_IF_DECIDE",
  "EV_IF_CONSEQUENT", "EV_IF_ALTERNATIVE", "EV_ASSIGNMENT_1",
  "EV_DEFINITION_1", "EV_APPL_DID_OPERATOR", "EV_APPL_ACCUMULATE_ARG",
  "EV_APPL_ACCUM_LAST_ARG", "EV_SEQUENCE_CONTINUE", "EVAL_DISPATCH",
  "EV_SELF_EVAL", "EV_VARIABLE", "EV_QUOTED", "EV_IF", "EV_ASSIGNMENT",
  "EV_DEFINITION", "EV_LAMBDA", "EV_NLAMBDA", "EV_BEGIN", "EV_APPLICATION",
  "EV_APPL_OPERAND_LOOP", "EV_APPL_LAST_ARG", "EV_SEQUENCE",
  "EV_SEQUENCE_LAST_EXP", "APPLY_DISPATCH", "PRIMITIVE_APPLY",
  "COMPOUND_APPLY", "MACRO_APPLY", "UNKNOWN_PROCEDURE_TYPE",
  "UNKNOWN_EXPRESSION_TYPE" };

Obj BROKEN_HEART = MAKE_BROKEN(0);

int is_integer(double f) { return f == trunc(f); }

void display2(Obj expr, int dotted, int level) {
  if (level > 10) {
    printf("...");
    return;
  }

  if (IS_SEXPR(expr)) {
    if (is_nil(expr)) {
      printf("()");
    } else {
      if (EQ(car(expr), PROCEDURE_SYM)) { 
	printf("<λ");
	display2(cadr(expr), 0, level+1);
	printf(".");
	display2(caddr(expr), 0, level+1);
	printf(">");
      } if (!dotted)
	printf("(");
      display2(car(expr), 0, level+1);
      if (!is_nil(cdr(expr))) {
	printf(" ");
	if (!IS_SEXPR(cdr(expr)))
	  printf(". ");
	display2(cdr(expr), 1, level+1);
      }
      if (!dotted)
	printf(")");
    }
  } else if (IS_SYMBOL(expr) || IS_STR(expr)) {
    printf("%s", find(NAN_VALUE(expr)));
  } else if (IS_BROKEN(expr)) {
    printf("BBBB (%llx)", NAN_VALUE(expr));
  } else if (IS_PRIM(expr)) {
    printf("<primitive #%lld>", NAN_VALUE(expr));
  } else if (IS_ARRAY(expr)) {
    printf("<array>");
  } else if (IS_DOUBLE(expr)) {
    double x = DOUBLEVALUE(expr);
    if (is_integer(x)) {
      if (fabs(x) < INT_MAX)
	printf("%.0f", x);	/* show as e.g. 62 */
      else
	printf("%e", x);	/* show as e.g. 3.14e12 */
    } else
      printf("%f", x);		/* show as e.g. 3.14 */
  } else {
    fprintf(stderr, "display2: can't happen: %llx\n", expr.as_int);
    exit(1);
  }
}

void display(Obj expr) {
  display2(expr, 0, 0);
}

void display_registers(char *where) {
  if (verbose != 1)
    return;
  printf("===============================%s (%d)=\n", where, label);
  printf("expr: "); display(expr); NL;
  printf("env:  "); display(env); NL;
  printf("cont: "); printf("%s\n", continuation_string[NAN_VALUE(cont)]);
  printf("val:  "); display(val); NL;
  printf("unev: "); display(unev); NL;
  printf("argl: "); display(argl); NL;
  printf("proc: "); display(proc); NL;
  printf("stack:"); display(stack); NL;
  printf("========================================\n");
}

void dumpval(Obj n) {
  if (IS_SEXPR(n)) {
    printf("P%lld ", NAN_VALUE(n));
  } else if (IS_SYMBOL(n)) {
    printf("%s ", find(NAN_VALUE(n)));
  } else if (IS_BROKEN(n)) {
    printf("$%lld ", NAN_VALUE(n));
  } else if (IS_PRIM(n)) {
    printf("prim%lld ", NAN_VALUE(n));
  } else {
    printf("?%llx ", NAN_VALUE(n));
  }
}

void dump_memory2(int from, int to, Obj *cars, Obj *cdrs, char *carstr, char *cdrstr) {
  int i; 
  printf("      ");
  for (i = from; i <= to; i++) {
    printf("%d:\t", i);
  }
  printf("\n");
  printf("%s: ", carstr);
  for (i = from; i <= to; i++) {
    dumpval(cars[i]);
    printf("\t");
  }
  printf("\n%s: ", cdrstr);
  for (i = from; i <= to; i++) {
    dumpval(cdrs[i]);
    printf("\t");
  }
  printf("\n");
}

char *cellname(Obj p) {
  char *name = malloc(80);
  if (is_nil(p))
    return "-";
  if (IS_SEXPR(p)) {
    sprintf(name, "P%lld", NAN_VALUE(p));
    return name;
  } else if (IS_SYMBOL(p)) {
    sprintf(name, "%s", find(NAN_VALUE(p)));
    if (name[0] == '<')
      return "\\<";
    else if (name[0] == '>')
      return "\\>";
    return name;
  } else if (IS_STR(p)) {
    sprintf(name, "\"%s\"", find(NAN_VALUE(p)));
    return name;
  } else if (IS_BROKEN(p)) {
    sprintf(name, "proc %lld", NAN_VALUE(p));
    return name;
  } else if (IS_PRIM(p)) {
    sprintf(name, "prim %lld ", NAN_VALUE(p));
    return name;
  } else {
    return "?";
  }
}

int gnr = 0;

void mkgraph() {
  int i;
  char gname[80];
  sprintf(gname, "mem-%d.dot", gnr++);
  FILE *fp = fopen(gname, "w");

  fprintf(fp, "digraph T {\n");
  fprintf(fp, "page=\"8,11!\";\n");
  fprintf(fp, "ratio=compress;\n");
  fprintf(fp, "margin=0;\n");
  fprintf(fp, "rankdir=LR\n");
  fprintf(fp, "node [shape=record]\n\n");  

  for (i = 0; i < free_index; i++)
    fprintf(fp, "%d [label=\"<car>%s | <cdr>%s\"]\n", i, cellname(thecars[i]), cellname(thecdrs[i]));

  fprintf(fp, "env  [style=filled, color=\"cyan\"];\n");
  fprintf(fp, "val  [style=filled, color=\"cyan\"];\n");
  fprintf(fp, "unev [style=filled, color=\"cyan\"];\n");
  fprintf(fp, "argl [style=filled, color=\"cyan\"];\n");
  fprintf(fp, "proc [style=filled, color=\"cyan\"];\n");
  fprintf(fp, "expr [style=filled, color=\"cyan\"];\n");
  fprintf(fp, "prim_proc [style=filled, color=\"cyan\"];\n");
  fprintf(fp, "stack [style=filled, color=\"cyan\"];\n");
  fprintf(fp, "conscell [style=filled, color=\"cyan\"];\n");
  fprintf(fp, "cont [style=filled, color=\"cyan\"];\n");
  fprintf(fp, "tmp1 [style=filled, color=\"cyan\"];\n");
  fprintf(fp, "tmp2 [style=filled, color=\"cyan\"];\n");
  fprintf(fp, "tmp3 [style=filled, color=\"cyan\"];\n");

  fprintf(fp, "\n");
  for (i = 0; i < free_index; i++) {
    if (is_pair(thecars[i]) || is_compound(thecars[i]))
      fprintf(fp, "%d:car -> %llu:car\n", i, NAN_VALUE(thecars[i]));
    if (is_pair(thecdrs[i]) || is_compound(thecdrs[i]))
      fprintf(fp, "%d:cdr -> %llu:car\n", i, NAN_VALUE(thecdrs[i]));
  }

  fprintf(fp, "env -> %llud:car\n", NAN_VALUE(env));
  fprintf(fp, "val -> %llud:car\n", NAN_VALUE(val));
  fprintf(fp, "unev -> %llud:car\n", NAN_VALUE(unev));
  fprintf(fp, "argl -> %llud:car\n", NAN_VALUE(argl));
  fprintf(fp, "proc -> %llud:car\n", NAN_VALUE(proc));
  fprintf(fp, "expr -> %llud:car\n", NAN_VALUE(expr));
  fprintf(fp, "prim_proc -> %llud:car\n", NAN_VALUE(prim_proc));
  fprintf(fp, "stack -> %llud:car\n", NAN_VALUE(stack));
  fprintf(fp, "conscell -> %llud:car\n", NAN_VALUE(conscell));
  fprintf(fp, "cont -> %llud:car\n", NAN_VALUE(cont));
  fprintf(fp, "tmp1 -> %llud:car\n", NAN_VALUE(tmp1));
  fprintf(fp, "tmp2 -> %llud:car\n", NAN_VALUE(tmp2));
  fprintf(fp, "tmp3 -> %llud:car\n", NAN_VALUE(tmp3));
  fprintf(fp, "}\n");
  fclose(fp);
}

int min(int x, int y) { return x < y ? x : y; }

void dump_memory() {
  mkgraph();
  printf("MEMORY==> env = %llu, prim_proc = %llu free_index = %d\n", NAN_VALUE(env), NAN_VALUE(prim_proc), free_index);
  int i, last;
  for (last = MEMSIZE; last >= 0; last--)
    if (!is_nil(thecars[last]))
      break;
  for (i = 0; i < last+11; i+= 12) {
    if (i < MEMSIZE) {
      dump_memory2(i, min(i+11, MEMSIZE-1), thecars, thecdrs, "tcar", "tcdr");
      NL;
    }
  }
  for (i = 0; i < last+11; i+= 12) {
    if (i < MEMSIZE) {
      dump_memory2(i, min(i+11, MEMSIZE-1), newcars, newcdrs, "ncar", "ncdr");
      NL;
    }
  }
}

// Assumes little endian
void printBits(size_t const size, void const * const ptr) {
  unsigned char *b = (unsigned char*) ptr;
  unsigned char byte;
  int i, j;

  for (i = size-1; i >= 0; i--) {
    for (j = 7; j >= 0; j--) {
      byte = (b[i] >> j) & 1;
      printf("%u", byte);
    }
    printf(" ");
  }
  puts("");
}
