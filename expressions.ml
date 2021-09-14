(* https://en.wikipedia.org/wiki/Parsing_expression_grammar *)


type tok = Litt of int
		 | Var  of string
		 | Expr of tok list 
		 | Term of tok list 
		 | Fact of tok list

type expr_parser = (char list, tok list) parser;;


let token_litt : expr_parser = fun input -> 
	match parse_int input with 
		| rem, h::t -> rem, [[Litt(int_of_charlist h)]]
		| rem, []	   -> rem, []
;;

let token_plus : expr_parser = function
	| h::t when h = '+' -> t, [[]]
	| input -> input, []
;;

let token_mult : expr_parser = function
	| h::t when h = '*' -> t, [[]]
	| input -> input, []
;;

let token_lpar : expr_parser = function
	| h::t when h = '(' -> t, [[]]
	| input -> input, []
;;

let token_rpar : expr_parser = function
	| h::t when h = ')' -> t, [[]]
	| input -> input, []
;;




(*  Defined recursively
 *	expr := term + term
 *	term := fact * fact | fact
 *	fact := integer | (expr)
 *)

let rec expr input = 
		match ((term <*> token_plus <*> term) <|> term) input with
			| rem, [] -> rem, []
			| rem, res -> rem, [[Expr(flatten res)]]

	and term input = 
		match ((fact <*> token_mult <*> fact) <|> fact) input with
			| rem, [] -> rem, []
			| rem, res -> rem, [[Term(flatten res)]]

	and fact input = 
		match ((token_lpar <*> expr <*> token_rpar) <|> token_litt) input with
			| rem, [] -> rem, []
			| rem, res -> rem, [[Fact(flatten res)]]


;;


let rec evaluate = function 
	| Expr(e) -> 0
	| Term(e) -> 1
	| Fact(e) -> 2
;;