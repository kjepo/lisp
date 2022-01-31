OUT      = lisp
CFILES   = $(wildcard *.c)
OBJFILES = $(CFILES:.c=.o)

CC      = gcc
CFLAGS  = -g -I/opt/homebrew/opt/readline/include/
LDLIBS  = -lreadline
LDFLAGS ="-L/opt/homebrew/opt/readline/lib"


$(OUT): $(OBJFILES)

.PHONY: clean
clean:
	rm -f $(OBJFILES) $(OUT) *~ core print.ps

print:
	enscript -2Gr -o print.ps *.c

git:
	git add *.[ch] Makefile
	git commit -m "Automatic update"
	git push
