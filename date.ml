(* Date in HH:MM:SS 24h format *)
let pdate =
	let sep = parse_char ':'
	and two  = parse_char '2'
	and p0_9 = parse_any_chars ['0';'1';'2';'3';'4';'5';'6';'7';'8';'9']
	and p0_5 = parse_any_chars ['0';'1';'2';'3';'4';'5']
	and p0_3 = parse_any_chars ['0';'1';'2';'3']
	and p0_1 = parse_any_chars ['0';'1']
	in 

	((p0_1 <*> p0_9) <|> (two <*> p0_3)) (* Hours *)
	<*> sep <*> p0_5 <*> p0_9 (* Minutes *)
	<*> sep <*>	p0_5 <*> p0_9 (* Seconds *)
;;

let is_valid_date str =
	let rem, res = pdate (string_to_list str) in 
	rem = [] && res != []
;;