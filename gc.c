#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "lisp.h"
#include "gc.h"
#include "print.h"

void gc() {
  int scan, alloc, i;		// indices into newcars/newcdrs

  update_rootset();
  scan = alloc = 1;		// don't start at 0 (=NIL)
  memset(newcars, 0, MEMSIZE);
  memset(newcdrs, 0, MEMSIZE);
  // create cons(root, NIL) in newcars/newcdrs
  newcars[alloc] = thecars[objval(root)];
  newcdrs[alloc++] = thecdrs[objval(root)];

  while (alloc > scan) {
    Obj p1 = newcars[scan];  // (p1 . p2) is the cons cell at the scan pointer 
    if (is_pair(p1)) {	     // first handle p1
      i = objval(p1);	     // i is the array index "memory address" for p1
      if (thecars[i] == BROKEN_TAG) {      // p1 has moved 
        newcars[scan] = thecdrs[i];        // use forwarding address 
      } else {
	newcars[alloc] = thecars[i];       // move object at p1
	newcdrs[alloc] = thecdrs[i];       // move object at p1 
	newcars[scan] = mkpointer(alloc);  // update address in scan object
	thecars[i] = BROKEN_TAG;           // mark as moved 
	thecdrs[i] = mkpointer(alloc);	   // leave a forwarding address 
	alloc++;
      }
    }
    Obj p2 = newcdrs[scan];  // (p1 . p2) is the cons cell at the scan pointer
    if (is_pair(p2)) {	     // p1 processed, now handle p2
      i = objval(p2);        // i is the array index "memory address" for p2
      if (thecars[i] == BROKEN_TAG) {      // p2 has moved
	newcdrs[scan] = thecdrs[i];	   // use forwarding address
      } else {
	newcars[alloc] = thecars[i];       // move object at p2 
	newcdrs[alloc] = thecdrs[i];       // move object at p2
	newcdrs[scan] = mkpointer(alloc);  // update address in scan object
	thecars[i] = BROKEN_TAG ;          // mark as moved 
	thecdrs[i] = mkpointer(alloc);	   // leave a forwarding address
	alloc++;
      }
    }
    scan++;			           // finished with (p1 . p2) 
  }
  printf("[GC: compressed %d cells down to %d]\n", MEMSIZE, alloc);
  if (alloc >= MEMSIZE) {
    fprintf(stderr, "Sorry - memory is full, even after GC\n");
    exit(1);
  }

  free_index = alloc;
  Obj *t = thecars;  thecars = newcars;  newcars = t;
       t = thecdrs;  thecdrs = newcdrs;  newcdrs = t;
  restore_rootset();
  return;
}

