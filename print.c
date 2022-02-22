#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>
#include "lisp.h"
#include "hashtab.h"
#include "print.h"

void display2(Obj expr, int dotted) {
  if (expr == BROKEN_TAG) {
    printf("XXX");
    return;
  }
  switch (objtype(expr)) {
  case PAIR_TAG:
    if (expr == NIL)
      printf("()");
    else {
      if (car(expr) == PROCEDURE_SYM) { 
	printf("<λ"); display(cadr(expr)); printf("."); display(caddr(expr)); printf(">");
	break;
      } if (!dotted)
	printf("(");
      display(car(expr));
      if (cdr(expr) != NIL) {
	printf(" ");
	if (objtype(cdr(expr)) != PAIR_TAG)
	  printf(". ");
	display2(cdr(expr), 1);
      }
      if (!dotted)
	printf(")");
    }
    break;
  case SYMBOL_TAG:
  case STR_TAG:
    printf("%s", find(objval(expr)));
    break;
  case NUM_TAG:
    printf("%d", objval(expr));
    break;
  case BROKEN_TAG:
    printf("XXX ");
    break;
  case PRIM_TAG:
    printf("<primitive #%d>", objval(expr));
    break;
  case BOOL_TAG:
    printf("%s", (expr == False ? "#f" : "#t"));
    break;
  case ARRAY_TAG:
    printf("<array>");
    break;
  default:
    fprintf(stderr, "display2: can't happen: %d\n", objtype(expr));
    exit(1);
  }
}

void display(Obj expr) {
  display2(expr, 0);
}

void display_registers(char *where) {
  if (!verbose)
    return;
  printf("===============================%s=\n", where);
  printf("expr: "); display(expr); NL;
  printf("env:  "); display(env); NL;
  printf("cont: "); printf("%s\n", continuation_string[objval(cont)]);
  printf("val:  "); display(val); NL;
  printf("unev: "); display(unev); NL;
  printf("argl: "); display(argl); NL;
  printf("proc: "); display(proc); NL;
  printf("========================================\n");
}

void dumpval(Obj n) {
  if (n == BROKEN_TAG) {
    printf("XXX ");
    return;
  }
  switch (objtype(n)) {
  case PAIR_TAG:
    if (objval(n))
      printf("P%d ", objval(n));
    else
      printf("   ");
    break;
  case NUM_TAG:
    printf("INT %d ", objval(n));
    break;
  case SYMBOL_TAG:
    printf("%s ", find(objval(n)));
    break;
  case BROKEN_TAG:
    printf("$%d ", objval(n));
    break;
  case PRIM_TAG:
    printf("prim%d ", objval(n));
    break;
  default:
    printf("?%d ", objval(n));
    break;
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
  if (p == 0)
    return "-";
  switch (objtype(p)) {
  case PAIR_TAG:
    sprintf(name, "P%d", objval(p));
    return name;
  case SYMBOL_TAG:
    sprintf(name, "%s", find(objval(p)));
    if (name[0] == '<')
      return "\\<";
    else if (name[0] == '>')
      return "\\>";
    return name;
  case BOOL_TAG:
    sprintf(name, "%s", objval(p) == False ? "#f" : "#t");
    return name;
  case STR_TAG:
    sprintf(name, "\"%s\"", find(objval(p)));
    return name;
  case NUM_TAG:
    sprintf(name, "int %d", objval(p));
    return name;
  case BROKEN_TAG:
    sprintf(name, "proc %d", objval(p));
    return name;
  case PRIM_TAG:
    sprintf(name, "prim %d ", objval(p));
    return name;
  default:
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
      fprintf(fp, "%d:car -> %d:car\n", i, objval(thecars[i]));
    if (is_pair(thecdrs[i]) || is_compound(thecdrs[i]))
      fprintf(fp, "%d:cdr -> %d:car\n", i, objval(thecdrs[i]));
  }

  fprintf(fp, "env -> %d:car\n", objval(env));
  fprintf(fp, "val -> %d:car\n", objval(val));
  fprintf(fp, "unev -> %d:car\n", objval(unev));
  fprintf(fp, "argl -> %d:car\n", objval(argl));
  fprintf(fp, "proc -> %d:car\n", objval(proc));
  fprintf(fp, "expr -> %d:car\n", objval(expr));
  fprintf(fp, "prim_proc -> %d:car\n", objval(prim_proc));
  fprintf(fp, "stack -> %d:car\n", objval(stack));
  fprintf(fp, "conscell -> %d:car\n", objval(conscell));
  fprintf(fp, "cont -> %d:car\n", objval(cont));
  fprintf(fp, "tmp1 -> %d:car\n", objval(tmp1));
  fprintf(fp, "tmp2 -> %d:car\n", objval(tmp2));
  fprintf(fp, "tmp3 -> %d:car\n", objval(tmp3));
  fprintf(fp, "}\n");
  fclose(fp);
}

int min(int x, int y) { return x < y ? x : y; }

void dump_memory() {
  mkgraph();
  printf("MEMORY==> env = %d, prim_proc = %d free_index = %d\n", objval(env), objval(prim_proc), free_index);
  int i, last;
  for (last = MEMSIZE; last >= 0; last--)
    if (thecars[last])
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
