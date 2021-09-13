 
(* The regex is a special case of parser where it only deals with
 * lists of chars, where the empty list [] is the empty word empsilon or "".
 *)

type regex = (char, char list) parser;;

(* Versions with explicite regex type *)
let parse_char : char->regex = canon;;
let epsilon    : regex = empty;;

let parse_any_chars l : regex =	parse_any_of (map parse_char l);;
let parse_all_chars l : regex =	parse_all_of (map parse_char l);;


let parse_string str  : regex = clump (parse_all_chars (string_to_list str));;


(* Elemtary sets *)
let p0__1 = parse_any_chars ['0'; '1'];;
let p0__7 = parse_any_chars (string_to_list "01234567");;
let p0__9 = parse_any_chars (string_to_list "0123456789");;
let p0__F = parse_any_chars (string_to_list "0123456789ABCDEF");;
let pa__z = parse_any_chars (string_to_list "abcdefghijklmnopqrstuvwxyz");;
let pA__Z = parse_any_chars (string_to_list "ABCDEFGHIJKLMNOPQRSTUVWXYZ");;

let latin    : regex = pa__z <|> pA__Z;;
let alphanum : regex = latin <|> p0__9;;

let space = parse_char ' ';;


(* Utility *)

let parse_int   : regex = clump (plus p0__9);;
let parse_float : regex = parse_int <*> (parse_char '.') <*> parse_int;;


let identifier : regex = latin <*> (star alphanum);;

let padded (p : regex) : regex = (star space) <*> p <*> (star space);;