all:	clean y.tab.c lex.yy.c
	gcc lex.yy.c y.tab.c -ly -lfl -o a

y.tab.c:
	bison -y -d parser.y

lex.yy.c:
	flex scanner.l

clean:
	rm -f a lex.yy.c y.tab.c y.tab.h
