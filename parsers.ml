

(* INITIAL DEFINITIONS *)

(* I define a parser as a function that consumes 
 * a single word input (a list of symbols)
 * and produces a list of what it found. 
 *)
type ('a, 'b) parser = 'a list -> 'a list * 'b list;;


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



let rec plus p : ('a, 'b) parser = fun input ->
	(* One or more *)
	((p <*> plus p) <|> p) input	
;;  (* BUG ? Does not work without applying input : infinite recursion *)


let star p : ('a, 'b) parser = 
	(* Zero or more *) 
	plus p <|> empty
;;



(* PARSER UTILITIES *)

let clump p : ('a, 'b) parser = fun input ->
	(* Utility function to regroup elements of output list *)
	let rem, res  =  p input in
	rem, [flatten res]
;;

(* create parsers with lists *)
let parse_any_of = assos empty (<|>);;
let parse_all_of = assos empty (<*>);; 



