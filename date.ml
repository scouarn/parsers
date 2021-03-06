
(* Time in HH:MM:SS 24h format 
 * Where HH <= 23, MM <= 59, SS <= 60 *)
let parse_time =
	let sep  = canon ':'
	and two  = canon '2'
	and p0_9 = parse_any_str "0123456789"
	and p0_5 = parse_any_str "012345"
	and p0_3 = parse_any_str "0123"
	and p0_1 = parse_any_str "01"
	in 

	((p0_1 <*> p0_9) <|> (two <*> p0_3)) (* Hours *)
	<*> sep <*> p0_5 <*> p0_9 (* Minutes *)
	<*> sep <*>	p0_5 <*> p0_9 (* Seconds *)
;;


(* Year in YYYY-MM-DD format *)
(* ACCEPTS INVALID DATES *)
let parse_date =
	let sep = canon '-'
	and p0_9 = parse_any_str "0123456789"
	in 

	p0_9 <*> p0_9 <*> p0_9 <*> p0_9 (* Year *)
	<*> sep <*> 
	p0_9 <*> p0_9 (* Month *)
	<*> sep <*>
	p0_9 <*> p0_9 (* Day   *)
;;



(* Parse to integers *)


type time_t = {hour : int; min   : int; sec : int};;
type date_t = {year : int; month : int; day : int};;


(* Time in HH:MM:SS 24h format 
 * Where HH <= 23, MM <= 59, SS <= 60 *)
let parse_time_v2 =
	let sep = leave_out (canon ':')
	and two  = canon '2'
	and p0_9 = parse_any_str "0123456789"
	and p0_5 = parse_any_str "012345"
	and p0_3 = parse_any_str "0123"
	and p0_1 = parse_any_str "01"
	
	in 

	let p00_23 = transform (* Hours *)
		((p0_1 <*> p0_9) <|> (two <*> p0_3))
		(fun x -> [int_of_charlist x])
	
	and p00_59 = transform (* Minutes and seconds *)
		(p0_5 <*> p0_9)
		(fun x -> [int_of_charlist x])

	in
	
	transform
		(p00_23 <*> sep <*> p00_59 <*> sep <*> p00_59)
		(function h::m::s::[] -> [{hour = h; min = m; sec = s}] | _ -> [])
;;


(* Year in YYYY-MM-DD format *)
(* ACCEPTS INVALID DATES *)
let parse_date_v2 =
	let sep = leave_out (canon '-')
	and p0_9 = parse_any_str "0123456789"
	in 
	
	
	let year = transform
		(p0_9 <*> p0_9 <*> p0_9 <*> p0_9)
		(fun x -> [int_of_charlist x])

	and month_or_day = transform 
		(p0_9 <*> p0_9)
		(fun x -> [int_of_charlist x])

	in

	transform 
		(year <*> sep <*> month_or_day <*> sep <*> month_or_day)
		(function y::m::d::[] -> [{year = y; month = m; day = d}] | _ -> [])

;;