(* Utils *)
let rec map f = function 
	| [] -> []
	| h::t -> (f h)::(map f t)
;;

let rec assos el_neutre f = function
	| [] -> el_neutre
	| h::[] -> h
	| h::t  -> f h (assos f t)

	in aux l
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

let parse_string (str : string) : regex = 

	let rec aux = function
		| c::[] -> parse_char c
		| c::l  -> parse_char c <*> aux l
		| _ -> epsilon

	in aux (string_to_list str)
;;