OUT      = lisp
CFILES   = $(wildcard *.c)
OBJFILES = $(CFILES:.c=.o)

CC      = gcc
CFLAGS  = -g
#CFLAGS = -Wall -Werror -Wmissing-prototypes
#CFLAGS  = -Wall -I /additional/include/dir
#LDFLAGS = -L /additional/lib/dir
#LDLIBS  = -ldependency1 -ldependency2

$(OUT): $(OBJFILES)

.PHONY: clean
clean:
	rm -f $(OBJFILES) $(OUT) *~ core print.ps

print:
	enscript -2Gr -o print.ps *.c
