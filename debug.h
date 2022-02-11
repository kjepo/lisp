
void display2(Obj expr, int dotted) {
  switch (objtype(expr)) {
  case PAIR_TAG:
    if (expr == 0)
      printf("()");
    else {
      if (!dotted)
	printf("(");
      display(car(expr));
      if (cdr(expr) != 0) {
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
  case STR_TAG:
    printf("%s", find(objval(expr)));
    break;
  case NUM_TAG:
    printf("%d", objval(expr));
    break;
  case PROC_TAG:
    //    printf("<procedure %d>", objval(expr));
    printf("<");
    if (expr && car(objval(expr))) {
      printf("λ");
      display(car(objval(expr)));
    } else
      printf("NULL");
    printf(".");
    if (expr && cdr(objval(expr)) && cadr(objval(expr)))
      display(cadr(objval(expr)));
    else
      printf("NULL");
    printf(",env");
    /*
    if (expr && cdr(objval(expr)) && cddr(objval(expr)) && caddr(objval(expr)))
      display(caddr(objval(expr)));
    else
      printf("NULL");
    */
    printf(">");
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
  printf("cont: "); printf("%s\n", continuation_string[cont]);
  printf("val:  "); display(val); NL;
  printf("unev: "); display(unev); NL;
  printf("argl: "); display(argl); NL;
  printf("proc: "); display(proc); NL;
  printf("stack ");
  for (int i = 0; i < StackPtr; i++) {
    printf("[");
    display(Stack[i]);
    printf("] ");
  }
  NL;
  printf("========================================\n");
}

void dumpval(Obj n) {
  switch (objtype(n)) {
  case PAIR_TAG:
    if (objval(n))
      printf("P%d ", objval(n));
    else
      printf("NIL");
    break;
  case NUM_TAG:
    printf("INT %d ", objval(n));
    break;
  case SYMBOL_TAG:
    printf("A%d ", objval(n));
    break;
  case PROC_TAG:
    printf("$%d ", objval(n));
    break;
  }
}


void dump_memory2(int from, int to) {
  int i; 
  printf("      ");
  for (i = from; i <= to; i++) {
    printf("%d:\t", i);
  }
  printf("\n");
  printf("carS: ");
  for (i = from; i <= to; i++) {
    dumpval(car(i));
    printf("\t");
  }
  printf("\ncdrS: ");
  for (i = from; i <= to; i++) {
    dumpval(thecdrs[i]);
    printf("\t");
  }
  printf("\n");
}

void dump_memory() {
  printf("env = %d\n", (int) env);
  int i, last;
  for (last = MEMSIZE; last >= 0; last--)
    if (car(last))
      break;
  for (i = 0; i < last; i+= 12) {
    dump_memory2(i, i+12);
    //    NL;
  }
}

// Assumes little endian
void printBits(size_t const size, void const * const ptr)
{
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
