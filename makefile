YFLAGS = -dv -t
CFLAGS = -g -Wformat=0 -w
a.out: grammar.o lex.o
	gcc -o a.out grammar.o lex.o driver.c -lfl -w
	rm -f *.o *~ y.output y.yab.h

grammar.o: grammar.y
lex.o: lex.l


