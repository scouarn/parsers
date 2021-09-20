(* https://en.wikipedia.org/wiki/Parsing_expression_grammar *)


type tok = Litt of int
		 | Expr of tok list 
		 | Term of tok list
		 | Fact of tok list

type expr_parser = (char list, tok list) parser;;



(* Automate parsing with input type different from output  *)
let transform p func : expr_parser = fun input -> 
	match p input with 
		| rem, [] -> rem, []
		| rem, res -> rem, [func res]
;;



(* h::t = [['1';'2';'3';...]] *)
let token_litt = transform parse_int 
	(function h::t -> [Litt (int_of_charlist h)] | _ -> []);;

(* Return [] because these tokens appear implicitly in the tree 
 * [] disappear with list flattening later *)
let token_plus = leave_out (parse_char '+');;
let token_mult = leave_out (parse_char '*');;
let token_lpar = leave_out (parse_char '(');;
let token_rpar = leave_out (parse_char ')');;


(*  Defined recursively
 *	expr := term ('+' term)*
 *	term := fact ('*' fact)*
 *	fact := [0..9]+ | '(' expr ')'
 *)
let rec expr input = 
		(transform (term <*> star (token_plus <*> term))
	 	(fun x -> [Expr (flatten x)])  ) input

	and term input = 
		(transform ((fact <*> star (token_mult <*> fact))) 
		(fun x -> [Term (flatten x)])  ) input

	and fact input = 
		(transform ((token_lpar <*> expr <*> token_rpar) <|> token_litt)
		(fun x -> [Fact (flatten x)])  ) input
;;


(* Calculates the resulting integer *)

let rec eval = function 
	| Expr terms -> assos 0 ( + ) (map eval terms)
	| Term facts -> assos 1 ( * ) (map eval facts)
	| Fact [sub_expr] -> eval sub_expr
	| Litt n -> n

	| _ -> failwith "Couldn't evaluate expression, invalid input three (bug !!!)."
;;


let eval_str str = 
	match expr (string_to_list str) with
	| [], [[Expr e]] -> eval (Expr e)
	| _ -> failwith "Couldn't evaluate expression, this is not a valid expression."
;;