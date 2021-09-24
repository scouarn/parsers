
module Regex = struct


type regex_tok 	= Alt  of regex_tok list
				| Seq  of regex_tok list
				| Star of regex_tok
			 	| Litt of char


let tok_pipe  = leave_out (parse_char '|')
let tok_star = leave_out (parse_char '*')
let tok_plus = leave_out (parse_char '+')
let tok_any  = leave_out (parse_char '.')

let tok_lpar = leave_out (parse_char '(')
let tok_rpar = leave_out (parse_char ')')

let tok_litt = transform alphanum
	(function [x] -> [Litt x] | _ -> [])



let rec tok_regex input =

		(tok_alt <*> star (tok_pipe <*> tok_alt))
		input

	and	tok_alt input = 

		(transform 
		(plus (tok_multi <|> tok_seq))
 		(fun x -> [Alt x]))
		input

	and tok_seq input = 

		(transform 
		((tok_lpar <*> tok_regex <*> tok_rpar) <|> tok_litt)
 		(fun x -> [Seq x]))
		input

	and tok_multi input = 

		(transform 
		(tok_seq <*> tok_star)
 		(function [Seq ([x])] -> [Star x] | _ -> []))
		input


let compile_regex = ()

end