type packet = Lit of int * int * int list | Op of int * int * int * packet list

let hextobin str = 
	let m = String.length str in
	let l = ref [] in
	for i = 0 to m-1 do
		let x = ref (int_of_string ("0x"^(String.sub str i 1))) in
		let s = ref [] in
		for j = 3 downto 0 do
			s := (if !x mod 2 = 1 then "1" else "0") :: !s;
			x := !x / 2;
		done;
		l := (String.concat "" !s) :: !l;
	done;
	String.concat "" (List.rev !l)


let rec parse inp p = 
	let ver = int_of_string ("0b"^(String.sub inp p 3)) in
	let ptyp = int_of_string ("0b"^(String.sub inp (p+3) 3)) in
	if ptyp = 4 then
		let loop = ref true in
		let pos = ref (p+6) in
		let l = ref [] in
		while !loop do
			let dat = String.sub inp !pos 5 in
			loop := dat.[0] = '1';
			l := (int_of_string ("0b"^(String.sub dat 1 4))) :: !l;
			pos := !pos + 5;
		done;
		((Lit (ver, ptyp, List.rev !l)), !pos)
	else
		let ltyp = int_of_string ("0b"^(String.sub inp (p+6) 1)) in
		if ltyp = 1 then
			let num = int_of_string ("0b"^(String.sub inp (p+7) 11)) in
			let pos = ref (p+18) in
			let l = ref [] in
			for i = 1 to num do
				let (pac, p) = parse inp !pos in
				pos := p;
				l := pac :: !l;
			done;
			(Op (ver, ptyp, ltyp, List.rev !l), !pos)
		else
			let len = int_of_string ("0b"^(String.sub inp (p+7) 15)) in
			let pos = ref (p+22) in
			let l = ref [] in
			while !pos < (p+22+len) do
				let (pac, p) = parse inp !pos in
				pos := p;
				l := pac :: !l;
			done;
			(Op (ver, ptyp, ltyp, List.rev !l), !pos)
	;;

let rec versum = function
	| Lit (a,_,_) -> a
	| Op (a,_,_,l) -> a + List.fold_left (+) 0 (List.map versum l)

let (x,_) = (parse (hextobin "005173980232D7F50C740109F3B9F3F0005425D36565F202012CAC0170004262EC658B0200FC3A8AB0EA5FF331201507003710004262243F8F600086C378B7152529CB4981400B202D04C00C0028048095070038C00B50028C00C50030805D3700240049210021C00810038400A400688C00C3003E605A4A19A62D3E741480261B00464C9E6A5DF3A455999C2430E0054FCBE7260084F4B37B2D60034325DE114B66A3A4012E4FFC62801069839983820061A60EE7526781E513C8050D00042E34C24898000844608F70E840198DD152262801D382460164D9BCE14CC20C179F17200812785261CE484E5D85801A59FDA64976DB504008665EB65E97C52DCAA82803B1264604D342040109E802B09E13CBC22B040154CBE53F8015796D8A4B6C50C01787B800974B413A5990400B8CA6008CE22D003992F9A2BCD421F2C9CA889802506B40159FEE0065C8A6FCF66004C695008E6F7D1693BDAEAD2993A9FEE790B62872001F54A0AC7F9B2C959535EFD4426E98CC864801029F0D935B3005E64CA8012F9AD9ACB84CC67BDBF7DF4A70086739D648BF396BFF603377389587C62211006470B68021895FCFBC249BCDF2C8200C1803D1F21DC273007E3A4148CA4008746F8630D840219B9B7C9DFFD2C9A8478CD3F9A4974401A99D65BA0BC716007FA7BFE8B6C933C8BD4A139005B1E00AC9760A73BA229A87520C017E007C679824EDC95B732C9FB04B007873BCCC94E789A18C8E399841627F6CF3C50A0174A6676199ABDA5F4F92E752E63C911ACC01793A6FB2B84D0020526FD26F6402334F935802200087C3D8DD0E0401A8CF0A23A100A0B294CCF671E00A0002110823D4231007A0D4198EC40181E802924D3272BE70BD3D4C8A100A613B6AFB7481668024200D4188C108C401D89716A080") 0)
let v = versum x;;

Printf.printf "%d\n" v

let rec maximum = function
	| [x] -> x
	| a::l -> let m = maximum l in if a > m then a else m
let rec minimum = function
	| [x] -> x
	| a::l -> let m = minimum l in if a < m then a else m

let rec eval = function
	| Lit (_,_,l) -> 
		let x = ref 0 in
		for i = 0 to List.length l - 1 do
			x := !x * 16 + List.nth l i;
		done;
		!x
	| Op (_,ptyp,_,l) ->
		if ptyp = 0 then
			List.fold_left (+) 0 (List.map eval l)
		else if ptyp = 1 then
			List.fold_left (fun a b-> a*b) 1 (List.map eval l)
		else if ptyp = 2 then
			minimum (List.map eval l)
		else if ptyp = 3 then
			maximum (List.map eval l)
		else if ptyp = 5 then
			let [a;b] = l in
			if (eval a) > (eval b) then 1 else 0
		else if ptyp = 6 then
			let [a;b] = l in
			if (eval a) < (eval b) then 1 else 0
		else if ptyp = 7 then
			let [a;b] = l in
			if (eval a) = (eval b) then 1 else 0
		else -1;;

Printf.printf "%d\n" (eval x)
