#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include "lisp.h"
#include "gc.h"

//  thecars                         newcars                         MEMSIZE
//  |                               |                               |
//  v                               v                               V
//  +-------------------------------+-------------------------------+
//  +                               |                               |
//  +-------------------------------+-------------------------------+

void gc() {

  Obj *newcars = thecars + MEMSIZE/2;
  Obj *newcdrs = thecdrs + MEMSIZE/2;
  Obj scan, alloc;		/* indices into newcars/newcdrs */

  printf("GARBAGE COLLECTING\n");

  update_rootset();
  scan = alloc = 0;
  printf("gc: root = %d\n", objval(root));

 // create cons(root, NIL) in newcars/newcdrs
  newcars[alloc] = root;
  newcdrs[alloc] = NIL;
  alloc++;

  return;
  while (alloc > scan) {
    Obj p1 = newcars[scan];
    Obj p2 = newcdrs[scan];
    if (is_pair((p1)))
	       ;
    
    
  }
    
  return;
}

