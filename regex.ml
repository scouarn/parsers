(* Apply f to every element *)
let rec map f = function 
	| [] -> []
	| h::t -> (f h)::(map f t)
;;

(* Apply x1 + x2 + x3... (+ = f) *)
let rec assos el_neutre f = function
	| [] -> el_neutre
	| h::[] -> h (* useful case ?*)
	| h::t  -> f h (assos el_neutre f t)
;;


(* string -> char list *)
let string_to_list str = 

	let rec aux l = function
		| 0 -> l
		| n -> aux ( (str.[n-1]) :: l) (n-1)

	in aux [] (String.length str)
;;



(* Parser defs *)
type 'a maybe = None | Just of 'a;;
type ('a, 'b) parser = 'a list -> 'a list * 'b maybe list;; (* maybe [Just of char], maybe [None], maybe [] (failure case) *)
type regex = (char, char) parser;;

(* Parse empty *)
let epsilon : regex = function
	| input -> input, [None]
;;

(* The first if successful, OR the second *)
let (<|>) (p1 : regex) (p2 : regex) : regex = fun input ->
	match p1 input with
	| rem, [] -> p2 input
	| x -> x
	
;;

(* The first AND the second *)
let (<*>) (p1 : regex) (p2 : regex) : regex = fun input ->
	match p1 input with
	| rem, [] -> input, []
	| rem1, res1 -> 
		match p2 rem1 with
		| rem2, [] -> input, []
		| rem2, res2 -> rem2, res1 @ res2
;;


(* One or more *)
let rec plus (p : regex) : regex = function input ->
	((p <*> plus p) <|> p) input	(* BUG ? Does not work without applying input *)
;;

(* Zero or more *) 
let rec star (p : regex) : regex = plus p <|> epsilon;;


(* Simple parsers *)
let parse_char (c : char) : regex = function
	| h::t when h = c -> t, [Just(h)]
	| input -> input, [] 
;;

let parse_any_of = assos epsilon (<|>);;
let parse_all_of = assos epsilon (<*>);; 

let parse_any_chars l =	parse_any_of (map parse_char l);;
let parse_all_chars l =	parse_all_of (map parse_char l);;

let parse_string s = parse_all_chars (string_to_list s);;
let parse_any_from_string s = parse_any_chars (string_to_list s);;

let digit = parse_any_from_string "0123456789";;
let parse_int   = plus digit;;
let parse_float = parse_int <*> (parse_char '.') <*> parse_int;;

let letter_lower = parse_any_from_string "abcdefghijklmnopqrstuvwxyz";;
let letter_upper = parse_any_from_string "ABCDEFGHIJKLMNOPQRSTUVWXYZ";;
let letter = letter_upper <|> letter_lower;;

let identifier = plus (letter <|> digit);;

let sspace = star (parse_char ' ');;

let padded p = sspace <*> p <*> sspace;;

let token_plus = padded (parse_char '+');;
let token_mult = padded (parse_char '*');;
let token_lpar = padded (parse_char '(');;
let token_rpar = padded (parse_char ')');;

(* To be defined via rec *)
let sum = parse_int <*> token_plus <*> parse_int;;
let prod = sum <*> token_mult <*> sum;;
let expr = prod <*> token_plus <*> prod;;





