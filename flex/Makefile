
all : main

run : main 
	./main


main : lex.yy.c scanner.flex
	gcc -o main lex.yy.c -lfl


lex.yy.c :
	flex scanner.flex


clean :
	rm -rf lex.yy.c main.c
