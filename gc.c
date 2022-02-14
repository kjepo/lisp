#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include "lisp.h"
#include "gc.h"

void gc() {
  printf("GARBAGE COLLECTING\n");
  Obj scan, alloc;		/* indices into newcars/newcdrs */

  // Update root set with current values for env, val, unev, argl, proc and expr
  car(root) = env;
  car(cdr(root)) = val;
  car(cdr(cdr(root))) = unev;
  car(cdr(cdr(cdr(root)))) = argl;
  car(cdr(cdr(cdr(cdr(root))))) = proc;
  car(cdr(cdr(cdr(cdr(cdr(root)))))) = expr;
  
  // create cons(root, NIL) in newcars/newcdrs
  newcars[0] = root;
  newcdrs[0] = NIL;
  alloc = 1;
  scan = 0;
  while (alloc > scan) {
    Obj p1 = newcars[scan];
    Obj p2 = newcdrs[scan];
    if (is_pair((p1)))
	       ;
    
    
  }
    
  return;
}

