fsource = expr.l


all : main

run : main 
	./main < input.txt


main : lex.yy.c
	gcc -o main lex.yy.c -lfl


lex.yy.c : $(fsource)
	flex $(fsource)


clean :
	rm -rf lex.yy.c main.c
