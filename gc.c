#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include "lisp.h"
#include "gc.h"
#include "print.h"

void gc() {
  Obj scan, alloc;		/* indices into newcars/newcdrs */
  //  printf("[GC]\n");
  //  dump_memory();
  /*
  printf("\nBefore: expr = "); display(expr); 
  printf(" val = "); display(val); 
  printf(" argl = "); display(argl);
  printf(" proc = "); display(proc);
  printf(" cont = "); display(cont);
  printf(" env = "); display(env);
  printf(" unev = "); display(unev); NL;
  */

  update_rootset();
  //  display(root);
  scan = alloc = 1;		/* don't start at 0 (=NIL) */
  // printf("gc: root = %d\n", objval(root));

  for (int i = 0; i < MEMSIZE; i++) {
    newcars[i] = 0;
    newcdrs[i] = 0;
  }

  // create cons(root, NIL) in newcars/newcdrs
  newcars[alloc] = thecars[objval(root)];
  newcdrs[alloc++] = thecdrs[objval(root)];

  //  dump_memory();

  while (alloc > scan) {
    // printf("processing object at scan=%d\n", scan);
    Obj p1 = newcars[scan];           /* (p1 . p2) is the cons cell at the scan pointer - first handle p1 */
    if (is_pair(p1)) {
      p1 = objval(p1);		/* int */
      // printf("p1 = %d\n", p1); 
      if (thecars[p1] == BROKENHEART) { /* p1 has moved */
        newcars[scan] = thecdrs[p1];	/* use forwarding address */
      } else {
	//printf("[1] copying from %d, allocating new object at %d: %d . %d\n", p1, alloc,
	//       objval(thecars[p1]), objval(thecdrs[p1]));
	newcars[alloc] = thecars[p1];   /* move object at p1 */
	newcdrs[alloc] = thecdrs[p1];   /* move object at p1 */
	newcars[scan] = mkpair(alloc);  /* update address in scan object */ // mkpair -> mkpointer
	thecars[p1] = BROKENHEART;      /* mark as moved */
	thecdrs[p1] = mkpair(alloc);	/* leave a forwarding address */
	alloc++;
      }
    }
    Obj p2 = newcdrs[scan];           /* (p1 . p2) is the cons cell at the scan pointer - handle p2 */
    if (is_pair(p2)) {
      p2 = objval(p2);		/* p2 is an int */
      // printf("p2 = %d\n", p2); 
      if (thecars[p2] == BROKENHEART) { /* p2 has moved */
	newcdrs[scan] = thecdrs[p2];	/* use forwarding address */
      } else {
	//printf("[2] copying from %d, allocating new object at %d: %d . %d\n", p2, alloc,
	//       objval(thecars[p2]), objval(thecdrs[p2]));
	newcars[alloc] = thecars[p2];   /* move object at p2 */
	newcdrs[alloc] = thecdrs[p2];   /* move object at p2 */
	newcdrs[scan] = mkpair(alloc);	      /* update address in scan object */
	thecars[p2] = BROKENHEART;      /* mark as moved */
	thecdrs[p2] = mkpair(alloc);	      /* leave a forwarding address */
	alloc++;
      }
    }
    scan++;			/* finished with (p1 . p2) */
    // printf("scan = %d, alloc = %d\n", scan, alloc);
    //    dump_memory();
  }
  printf("[GC: compressed %d cells down to %d]\n", MEMSIZE, alloc);
  if (alloc > MEMSIZE - 1) {
    fprintf(stderr, "Sorry - memory is full, even after GC\n");
    exit(1);
  }

  free_index = alloc;
  Obj *t = thecars;
  thecars = newcars;
  newcars = t;
  t = thecdrs;
  thecdrs = newcdrs;
  newcdrs = t;

  restore_rootset();
  /*
  printf("\nAfter: expr = "); display(expr); 
  printf(" val = "); display(val); 
  printf(" argl = "); display(argl);
  printf(" proc = "); display(proc);
  printf(" cont = "); display(cont);
  printf(" env = "); display(env);
  printf(" unev = "); display(unev); NL;
  */

  // dump_memory();
  return;
}

