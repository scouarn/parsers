

(* INITIAL DEFINITIONS *)

(* I define a parser as a function that consumes 
 * a single word input (a list of symbols)
 * and produces a list of what it found. 
 *)
type 'a parser = char list -> char list * 'a list maybe;;


let canon e : 'a parser = function
	(* Create canonical parser from an element *)
	| h::t when h = e -> t, Just [h]
	| input -> input, None 
;;


let empty : 'a parser = fun
	(* Parse empty; it can always be found as prefix of anything *)
	input -> input, Just []
;;

let any : 'a parser = function
	(* Parse any; it can always be found *)

	| h::t -> t, Just [h]
	| [] -> [], None
;;



(* PARSER OPERATIONS *)


let (<|>) p1 p2 : 'a parser = fun input ->
	(* The first (if successful), OR the second *)
	match p1 input with
	| rem, None -> p2 input
	| x -> x
;;


let (<*>) p1 p2 : 'a parser = fun input ->
	(* The first AND the second *)
	match p1 input with 
	| rem, None -> input, None
	| rem1, Just res1 -> 
		match p2 rem1 with
		| rem2, None -> input, None
		| rem2, Just res2 -> rem2, Just(res1 @ res2)
;;


(* One or more *)
let rec plus p : 'a parser = fun input ->
	((p <*> plus p) <|> p) input	
;;  (* BUG ? Does not work without applying input : infinite recursion *)

(* One or zero *)
let question p : 'a parser = p <|> empty;;

(* Zero or more *) 
let star p : 'a parser = question (plus p);;
	


(* PARSER UTILITIES *)


let leave_out p : 'a parser = fun input ->
	(* Always puts Just [] and forget the result *)
	let rem, res = p input in
	if res = None then rem, None else rem, Just []
;;

let keep_in p : 'a parser = fun input ->
	(* Always puts back the input and do not 'eat' the element *)
	let rem, res = p input in
	input, res
;;

(* FOLD-RIGHT ? *)
(* create parsers from lists of parsers *)
let parse_any_of = assos empty (<|>);;
let parse_all_of = assos empty (<*>);; 

(* 
let parse_any_of plist = fun input -> (List.fold_right (<|>) plist empty) input;;
let parse_all_of plist = fun input -> (List.fold_right (<*>) plist empty) input;; 
 *)


(* Automate parsing with input type different from output  *)
let transform p func : 'a parser = fun input -> 
	match p input with 
		| rem, None -> rem, None
		| rem, Just res -> rem, Just (func res)
;;


let is_valid p = fun input ->
	let rem, res = p input in 
	rem = [] && res != None
;;


let app_to_str func str = func (string_to_list str);;






