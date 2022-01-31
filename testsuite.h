void test() {

#ifdef PRETEST
#endif
  
  // 42
  expr = mknum(42); eval();
  verify(objval(val) == 42);

  // (quote 17)
  expr = cons(QUOTE_SYM, cons(mknum(17), 0));  eval();
  verify(objval(val) == 17);

  // (quote (1 2 3))
  expr = cons(QUOTE_SYM, cons(cons(mknum(1), cons(mknum(2), cons(mknum(3), 0))), 0));
  eval();
  verify(objtype(val) == PAIR_TAG
	 && objval(CAR(val)) == 1
	 && objval(CADR(val)) == 2
	 && objval(CADDR(val)) == 3);

  // x
  expr = mksym("x"); eval();
  verify(objval(val) == 8);

  // (set! x 22)
  expr = cons(SETBANG_SYM, cons(mksym("x"), cons(mknum(22), 0))); eval();
  printf("---%d\n", objval(env_lookup(mksym("x"), env)));
  verify(objval(env_lookup(mksym("x"), env)) == 22);

  // (define z 29)
  expr = cons(DEFINE_SYM, cons(mksym("z"), cons(mknum(29), 0))); eval();
  verify(objval(env_lookup(mksym("z"), env)) == 29);

  // (define l123 (quote (1 2 3)))
  Obj l123 = cons(QUOTE_SYM, cons(cons(mknum(1), cons(mknum(2), cons(mknum(3), 0))), 0));
  expr = cons(DEFINE_SYM, cons(mksym("l123"), cons(l123, 0)));
  eval();

  // #t
  expr = mkbool("#t");
  eval();
  verify(val == True);

  // #f
  expr = mkbool("#f");
  eval();
  verify(val == False);

  // (if #t (quote a) (quote b)))
  Obj t1 = cons(QUOTE_SYM, cons(mksym("a"), 0));
  Obj t2 = cons(QUOTE_SYM, cons(mksym("b"), 0));
  expr = cons(IF_SYM, cons(mkbool("#t"), cons(t1, cons(t2, 0))));
  eval();
  verify(val == mksym("a"));

  // (if #f (quote a) (quote b)))
  expr = cons(IF_SYM, cons(mkbool("#f"), cons(t1, cons(t2, 0))));
  eval();
  verify(val == mksym("b"));

  // (if 42 (quote a) (quote b)))
  expr = cons(IF_SYM, cons(mknum(42), cons(t1, cons(t2, 0))));
  eval();
  verify(val == mksym("a"));
  
  
#ifdef BIGTEST
#endif 
  expr = cons(CAR_SYM, cons(mksym("l123"), 0));
  eval();
  

  // (car (quote (1 2 3)))
  expr = cons(CAR_SYM, cons(cons(QUOTE_SYM, cons(cons(mknum(1), cons(mknum(2), cons(mknum(3), 0))), 0)), 0));
  eval();

  // (cdr (quote (1 2 3)))
  expr = cons(CDR_SYM, cons(cons(QUOTE_SYM, cons(cons(mknum(1), cons(mknum(2), cons(mknum(3), 0))), 0)), 0));
  eval();
  
  // expr = mksym("l123");
  // eval();

  expr = cons(CAR_SYM, cons(mksym("l123"), 0));
  eval();

  expr = cons(CONS_SYM, cons(mknum(1), cons(mknum(2), 0)));
  eval();


/*
  Obj p = mkproc(mksym("x"), cons(mksym("x"), NIL), 99);
  printf("expr = %d %d\n", objval(p), objtype(p));
  expr = cons(p, NIL);
  */
}

