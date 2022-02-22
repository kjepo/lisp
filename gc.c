#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "lisp.h"
#include "gc.h"
#include "print.h"

int alloc;

void mscopy(from) {
  newcars[alloc] = thecars[from];
  newcdrs[alloc] = thecdrs[from];
  alloc++;
}

#define ROOTSET(x) newcars[alloc++] = x;
#define ROOTGET(x) x = newcars[alloc++];

int gc() {
  int i, scan;                             // indices into newcars/newcdrs
  scan = alloc = 1;                        // don't start at 0 (=NIL)
  memset(newcars, 0, MEMSIZE);  memset(newcdrs, 0, MEMSIZE);
  ROOTSET(env); ROOTSET(val); ROOTSET(unev); ROOTSET(argl); ROOTSET(proc); ROOTSET(expr); ROOTSET(cont);
  ROOTSET(stack); ROOTSET(conscell); ROOTSET(prim_proc); ROOTSET(tmp1); ROOTSET(tmp2); ROOTSET(tmp3); 
  while (alloc > scan) {
    Obj p1 = newcars[scan];                // (p1 . p2) is the cons cell at the scan pointer
    if (is_pair(p1)) {                     // first handle p1
      i = objval(p1);                      // i is the array index "memory address" for p1
      if (thecars[i] != BROKEN_TAG) {      // move p1
        newcars[scan] = mkpointer(alloc);  // update address in scan object
        mscopy(i);
        thecars[i] = BROKEN_TAG;           // mark as moved
        thecdrs[i] = newcars[scan];        // leave a forwarding address
      } else                               // p1 has already moved
        newcars[scan] = thecdrs[i];        // use forwarding address
    }
    Obj p2 = newcdrs[scan];
    if (is_pair(p2)) {                     // p1 processed, now handle p2
      i = objval(p2);
      if (thecars[i] != BROKEN_TAG) {
        newcdrs[scan] = mkpointer(alloc);
        mscopy(i);
        thecars[i] = BROKEN_TAG;
        thecdrs[i] = newcdrs[scan];
      } else
          newcdrs[scan] = thecdrs[i];
    }
    scan++;                               // finished with (p1 . p2)
  }
  printf("[GC: compressed %d cells down to %d]\n", MEMSIZE, alloc);
  free_index = alloc;
  alloc = 1;
  ROOTGET(env); ROOTGET(val); ROOTGET(unev); ROOTGET(argl); ROOTGET(proc); ROOTGET(expr); ROOTGET(cont);
  ROOTGET(stack); ROOTGET(conscell); ROOTGET(prim_proc); ROOTGET(tmp1); ROOTGET(tmp2); ROOTGET(tmp3); 
  Obj *t = thecars;  thecars = newcars;  newcars = t;
  t = thecdrs;  thecdrs = newcdrs;  newcdrs = t;
  return alloc >= MEMSIZE;
}
