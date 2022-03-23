CC	= gcc
# uncomment the following line if you are on e.g. Linux
CFLAGS	= -g -I/opt/homebrew/opt/readline/include/
LDFLAGS = -lreadline
DEPS	= gc.h hashtab.h print.h lisp.h
OBJ	= gc.o hashtab.o print.o lisp.o

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

lisp: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) $(LDFLAGS)

clean:
	rm -f $(OBJ) lisp *~ core print.ps

print:
	enscript -2Gr -o print.ps *.c

git:
	git add *.[ch] Makefile lib.scm
	git commit -m "Automatic update"
	git push
