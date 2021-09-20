 
(* Special case of parser where it only deals with
 * lists of chars, where the empty list [] is the empty word empsilon or "".
 *)

type char_parser = (char, char) parser;;

(* Versions with explicite type *)
let parse_char : char->char_parser = canon;;
let epsilon    : char_parser = empty;;

let parse_any_chars l : char_parser = parse_any_of (map parse_char l);;
let parse_all_chars l : char_parser = parse_all_of (map parse_char l);;

let parse_any_str str : char_parser = app_to_str parse_any_chars str;;
let parse_all_str str : char_parser = app_to_str parse_all_chars str;;


(* Elemtary sets *)
let binary      = parse_char '0' <|> parse_char '1';;
let octal       = parse_any_str "01234567";;
let decimal     = parse_any_str "0123456789";;
let hexadecimal = parse_any_str "0123456789ABCDEF";;
let lowerlatin  = parse_any_str "abcdefghijklmnopqrstuvwxyz";;
let upperlatin  = parse_any_str "ABCDEFGHIJKLMNOPQRSTUVWXYZ";;

let latin    : char_parser = lowerlatin  <|> upperlatin;;
let alphanum : char_parser = latin <|> decimal;;

let pspace = parse_char ' ';;
let pdot   = parse_char '.';;

(* Utility *)

let parse_int   : char_parser = plus decimal;;
let parse_float : char_parser = parse_int <*> pdot <*> parse_int;;
let identifier  : char_parser = latin <*> star alphanum;;

let padded (p : char_parser) : char_parser = star pspace <*> p <*> star pspace;;
