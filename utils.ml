
type 'a maybe = None | Just of 'a;;


let map = List.map;;
let len = List.length;;


(* Fold right (or left ??) with special case /!\ *)
let rec assos neutral f = function
	(* Apply x1 + x2 + x3... (+ = f) *)
	| [] -> neutral
	| h::[] -> h (* useful case ?*)
	| h::t  -> f h (assos neutral f t)
;;



let rec flatten = function 
	(* From list of lists to single list removes a single "level" *)
	| [] -> []
	| h::t -> h @ (flatten t)
;;




let string_to_list str = 
	(* string -> char list *)
	let rec aux l = function
		| 0 -> l
		| n -> aux ( (str.[n-1]) :: l) (n-1)

	in aux [] (String.length str)
;;



let rec string_of_list : char list -> string = function 
		| [] -> ""
		| h::t -> (String.make 1 h) ^ (string_of_list t);
;;

let int_of_charlist l : int = match string_of_list l with
	| ""  -> 0
	| s   -> int_of_string s
;;