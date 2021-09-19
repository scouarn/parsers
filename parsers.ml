

(* INITIAL DEFINITIONS *)

(* I define a parser as a function that consumes 
 * a single word input (a list of symbols)
 * and produces a list of what it found. 
 *)
type ('a, 'b) parser = 'a -> 'a * 'b list;;


let canon e : ('a, 'b) parser = function
	(* Create canonical parser from an element *)
	| h::t when h = e -> t, [[h]]
	| input -> input, [] 
;;


let empty : ('a, 'b) parser = fun
	(* Parse empty; it can always be found as prefix of anything *)
	input -> input, [[]]
;;


(* PARSER OPERATIONS *)


let (<|>) p1 p2 : ('a, 'b) parser = fun input ->
	(* The first (if successful), OR the second *)
	match p1 input with
	| rem, [] -> p2 input
	| x -> x
;;


let (<*>) p1 p2 : ('a, 'b) parser = fun input ->
	(* The first AND the second *)
	match p1 input with 
	| rem, [] -> input, []
	| rem1, res1 -> 
		match p2 rem1 with
		| rem2, [] -> input, []
		| rem2, res2 -> rem2, res1 @ res2
;;


(* One or more *)
let rec plus p : ('a, 'b) parser = fun input ->
	((p <*> plus p) <|> p) input	
;;  (* BUG ? Does not work without applying input : infinite recursion *)

(* One or zero *)
let question p : ('a, 'b) parser = p <|> empty;;

(* Zero or more *) 
let star p : ('a, 'b) parser = question (plus p);;
	


(* PARSER UTILITIES *)

let clump p : ('a, 'b) parser = fun input ->
	(* Utility function to regroup elements of output list *)
	let rem, res  =  p input in
	rem, [flatten res]
;;


let leave_out p : ('a, 'b) parser = fun input ->
	let rem, res = p input in
	if res = [] then rem, [] else rem, [[]]
;;

let keep_in p : ('a, 'b) parser = fun input ->
	let rem, res = p input in
	input, res
;;

(* FOLD-RIGHT ? *)
(* create parsers with lists *)
let parse_any_of = assos empty (<|>);;
let parse_all_of = assos empty (<*>);; 

(* 
let parse_any_of plist = fun input -> (List.fold_right (<|>) plist empty) input;;
let parse_all_of plist = fun input -> (List.fold_right (<*>) plist empty) input;; 
 *)


