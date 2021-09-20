(* https://en.wikipedia.org/wiki/Parsing_expression_grammar *)


type tok = Litt of int
		 | Expr of tok list 
		 | Term of tok list
		 | Fact of tok

type expr_parser = (char, tok) parser;;


(* Leave out because these tokens appear implicitly in 
 * the tree structure *)
let token_plus = leave_out (parse_char '+');;
let token_mult = leave_out (parse_char '*');;
let token_lpar = leave_out (parse_char '(');;
let token_rpar = leave_out (parse_char ')');;

let token_litt = transform parse_int 
	(fun x -> [Litt (int_of_charlist x)]);;


(*  Defined recursively
 *	expr := term ('+' term)*
 *	term := fact ('*' fact)*
 *	fact := [0..9]+ | '(' expr ')'
 *)
let rec expr input = 
		(transform (term <*> star (token_plus <*> term))
	 	(fun x -> [Expr x])  ) input

	and term input = 
		(transform ((fact <*> star (token_mult <*> fact))) 
		(fun x -> [Term x])  ) input

	and fact input = 
		(transform ((token_lpar <*> expr <*> token_rpar) <|> token_litt)
		(function [token] -> [Fact token] | _ -> [])  ) input
;;



let rec eval = function 
	(* Calculate the resulting integer *)
	| Expr terms -> assos 0 ( + ) (map eval terms)
	| Term facts -> assos 1 ( * ) (map eval facts)
	| Fact Expr sub_expr -> eval (Expr sub_expr)
	| Fact Litt n -> n

	| _ -> failwith "Invalid three structure (bug !!!)."
;;


let eval_str str = 
	(* Wrapper for strings *)
	match app_to_str expr str with
	| [], Just [Expr e] -> eval (Expr e)
	| _ -> failwith "Invalid input, this is not a valid expression."
;;


(* Parse expression with variables
 * and reduce it with known constants ? *)



let test_rules = 

	eval_str "10" = 10 &&
	
	eval_str "10+2" = 10+2 &&
	eval_str "10*2" = 10*2 &&
	
	eval_str "10+2+3" = 10+2+3 &&
	eval_str "10*2*3" = 10*2*3 &&
	
	eval_str "10+2*3" = 10+2*3 &&
	eval_str "10*2+3" = 10*2+3 &&
	
	eval_str "(10+2)*3" = (10+2)*3 &&
	eval_str "10+(2*3)" = 10+(2*3) &&

	eval_str "(10+2)+(3+5)" = (10+2)+(3+5) &&
	eval_str "(10+2)*(3+5)" = (10+2)*(3+5) &&
	eval_str "(10*2)+(3*5)" = (10*2)+(3*5) &&
	eval_str "(10*2)*(3*5)" = (10*2)*(3*5)
;;


