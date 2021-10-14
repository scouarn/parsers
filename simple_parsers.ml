
let parse_any_chars l = parse_any_of (map canon l);;
let parse_all_chars l = parse_all_of (map canon l);;

let parse_any_str str = app_to_str parse_any_chars str;;
let parse_all_str str = app_to_str parse_all_chars str;;


(* Elemtary sets *)
let binary      = parse_any_str "01";;
let octal       = parse_any_str "01234567";;
let decimal     = parse_any_str "0123456789";;
let hexadecimal = parse_any_str "0123456789ABCDEF";;
let lowerlatin  = parse_any_str "abcdefghijklmnopqrstuvwxyz";;
let upperlatin  = parse_any_str "ABCDEFGHIJKLMNOPQRSTUVWXYZ";;

let latin    = lowerlatin  <|> upperlatin;;
let alphanum = latin <|> decimal;;

let pspace = canon ' ';;
let pdot   = canon '.';;



(* Utility *)

let parse_int   = plus decimal;;
let parse_float = parse_int <*> pdot <*> parse_int;;
let identifier = latin <*> star alphanum;;

let padded p = star pspace <*> p <*> star pspace;;
