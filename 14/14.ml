let start1 = "NNCB"
let inp1 = [("CH","B");("HH","N");("CB","H");("NH","C");("HB","C");("HC","B");("HN","C");("NN","C");("BH","H");("NC","B");("NB","B");("BN","B");("BB","N");("BC","B");("CC","N");("CN","C")]
let start = "PBVHVOCOCFFNBCNCCBHK"
let inp = [("FV","C");("SS","B");("SC","B");("BP","K");("VP","S");("HK","K");("FS","F");("CC","V");("VB","P");("OP","B");("FO","N");("FH","O");("VK","N");("PV","S");("HV","O");("PF","F");("HH","F");("NK","S");("NC","S");("FC","H");("FK","K");("OO","N");("HP","C");("NN","H");("BB","H");("CN","P");("PS","N");("VF","S");("CB","B");("OH","S");("CF","C");("OK","P");("CV","V");("CS","H");("KN","B");("OV","S");("HB","C");("OS","V");("PC","B");("CK","S");("PP","K");("SN","O");("VV","C");("NS","F");("PN","K");("HS","P");("VO","B");("VC","B");("NV","P");("VS","N");("FP","F");("HO","S");("KS","O");("BN","F");("VN","P");("OC","K");("SF","P");("PO","P");("SB","O");("FN","F");("OF","F");("CP","C");("HC","O");("PH","O");("BC","O");("NO","C");("BH","C");("VH","S");("KK","O");("SV","K");("KB","K");("BS","S");("HF","B");("NH","S");("PB","N");("HN","K");("SK","B");("FB","F");("KV","S");("BF","S");("ON","S");("BV","P");("KC","S");("NB","S");("NP","B");("BK","K");("NF","C");("BO","K");("KF","B");("KH","N");("SP","O");("CO","S");("KO","V");("SO","B");("CH","C");("KP","C");("FF","K");("PK","F");("OB","H");("SH","C")]

let explode s = List.init (String.length s) (String.get s)
let implode l = String.init (List.length l) (List.nth l)

let rec insert rules = function
	| [] -> []
	| [a] -> [a]
	| a::b::str -> 
		let ret = ref [] in
		for i = 0 to List.length rules - 1 do
			let (r1,r2) = List.nth rules i in
			if String.init 2 (fun i-> if i=0 then a else b) = r1 then
				ret := a::(r2.[0])::(insert rules (b::str))
		done;
		if !ret = [] then
			a::(insert rules (b::str))
		else
			!ret;;

let rec ins n rules str = 
	if n = 0 then
		str
	else 
		ins (n-1) rules (insert rules str);;

let calcres n = 
	let l = (ins n inp (explode start)) in
	let count c l = List.length ((List.filter (fun x->x=c)) l) in
	let chars = List.sort_uniq (Char.compare) l in
	let min = ref (count (List.hd l) l) in
	let max = ref (count (List.hd l) l) in
	for i = 0 to List.length chars - 1 do
		let c = count (List.nth chars i) l in
		min := if c < !min then c else !min;
		max := if c > !max then c else !max
	done;
	!max - !min;;

Printf.printf "%d\n" (calcres 10);;

let parse str = 
	let st = ref [] in
	for i = 0 to String.length str - 2 do
		st := (implode [str.[i]; str.[i+1]], 1) :: !st;
	done;
	!st

let rec inc str n = function
	| [] -> [(str, n)]
	| s::states -> 
		let (a,b) = s in
		if a = str then (a,b+n) :: states else (a,b) :: inc str n states

let insert2 rules state = 
	let next = ref [] in
	for k = 0 to List.length state - 1 do
		let (a,b) = List.nth state k in
		for i = 0 to List.length rules - 1 do
			let (r1,r2) = List.nth rules i in
			if a = r1 then
				let s1 = implode [r1.[0]; r2.[0]] in
				let s2 = implode [r2.[0]; r1.[1]] in
			next := inc s1 b !next;
			next := inc s2 b !next;
		done;
	done;
	!next;;

let rec ins2 n rules state = 
	if n = 0 then
		state
	else 
		ins2 (n-1) rules (insert2 rules state);;

let count2 state = 
	let chars = List.sort_uniq (Char.compare) (explode (String.concat "" (List.map (fun (a,b)->a) state))) in
	let min = ref Int.max_int in
	let max = ref 0 in
	for i = 0 to List.length chars - 1 do
		let c = List.nth chars i in
		let s = ref 0 in
		for i = 0 to List.length state - 1 do
			let (a,b) = List.nth state i in
			if a.[0] = c then s := !s + b;
			if a.[1] = c then s := !s + b;
		done;
		s := (!s+1)/2;
		if !s > !max then max := !s;
		if !s < !min then min := !s;
	done;
	!max - !min;;

let st = ins2 40 inp (parse start);;
Printf.printf "%d\n" (count2 st)
