#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lisp.h"

int alloc;

void mscopy(int from) {
  newcars[alloc] = thecars[from];
  newcdrs[alloc] = thecdrs[from];
  alloc++;
}

#define ROOTSET(x) newcars[alloc++] = x;
#define ROOTGET(x) x = newcars[alloc++];

int gc() {
  Obj BROKEN_HEART = MAKE_BROKEN(0);
  int i, scan;                             // indices into newcars/newcdrs
  memset(newcars, 0, MEMSIZE);  memset(newcdrs, 0, MEMSIZE);
  scan = alloc = 1;                        // don't start at 0 (=NIL)
  ROOTSET(env); ROOTSET(val); ROOTSET(unev); ROOTSET(argl); ROOTSET(proc);
  ROOTSET(expr); ROOTSET(cont); ROOTSET(stack); ROOTSET(conscell);
  ROOTSET(prim_proc); ROOTSET(tmp1); ROOTSET(tmp2); ROOTSET(tmp3); 
  while (alloc > scan) {
    Obj p1 = newcars[scan];                // (p1 . p2) is the cons cell at the scan pointer
    if (is_pair(p1)) {                     // first handle p1
      i = NAN_VALUE(p1);                   // i is the array index "memory address" for p1
      if (!EQ(thecars[i], BROKEN_HEART)) { // move p1
	Obj addr = MAKE_SEXPR(alloc);
        newcars[scan] = addr;              // update address in scan object
        mscopy(i);
        thecars[i] = BROKEN_HEART;         // mark as moved
        thecdrs[i] = newcars[scan];        // leave a forwarding address
      } else                               // p1 has already moved
        newcars[scan] = thecdrs[i];        // use forwarding address
    }
    Obj p2 = newcdrs[scan];
    if (is_pair(p2)) {                     // p1 processed, now handle p2 in the same way
      i = NAN_VALUE(p2);
      if (!EQ(thecars[i], BROKEN_HEART)) {
	Obj addr = MAKE_SEXPR(alloc);
        newcdrs[scan] = addr;
        mscopy(i);
        thecars[i] = BROKEN_HEART;
        thecdrs[i] = newcdrs[scan];
      } else
	newcdrs[scan] = thecdrs[i];
    }
    scan++;                               // finished with (p1 . p2)
  }
  printf("[GC: compressed %d/%d cells down to %d/%d]\n", free_index, MEMSIZE, alloc, MEMSIZE);
  free_index = alloc;
  alloc = 1;                              // now retrieve the root set at index 1, 2, etc.
  ROOTGET(env); ROOTGET(val); ROOTGET(unev); ROOTGET(argl); ROOTGET(proc);
  ROOTGET(expr); ROOTGET(cont); ROOTGET(stack); ROOTGET(conscell); ROOTGET(prim_proc);
  ROOTGET(tmp1); ROOTGET(tmp2); ROOTGET(tmp3); 
  Obj *t = thecars;  thecars = newcars;  newcars = t;  // swap thecars/thecdrs with newcars/newcdrs
  t = thecdrs;  thecdrs = newcdrs;  newcdrs = t;
  return alloc >= MEMSIZE;                // return 1 if we're still out of memory
}
