let inp1 = [[5;4;8;3;1;4;3;2;2;3;];[2;7;4;5;8;5;4;7;1;1;];[5;2;6;4;5;5;6;1;7;3;];[6;1;4;1;3;3;6;1;4;6;];[6;3;5;7;3;8;5;4;7;8;];[4;1;6;7;5;2;4;6;4;5;];[2;1;7;6;8;4;1;7;2;1;];[6;8;8;2;8;8;1;1;3;4;];[4;8;4;6;8;4;8;5;5;4;];[5;2;8;3;7;5;1;5;2;6;]];;
let inp = [[2;5;2;4;2;5;5;3;3;1;];[1;1;3;5;6;2;5;8;8;1;];[2;8;3;8;3;5;3;8;6;3;];[1;6;6;2;3;1;2;3;6;5;];[6;8;4;7;8;3;5;8;2;5;];[2;1;8;5;6;8;4;3;6;7;];[6;8;7;4;2;1;2;8;3;1;];[5;3;8;7;2;4;7;8;1;1;];[2;2;5;5;4;8;2;8;7;5;];[8;5;2;8;5;5;7;1;3;1;]]

let inc oct = List.map (List.map (fun x->x+1)) oct

let rec inc1 row j = match row with
    | [] -> []
	| a::rest ->
    	if j > 0 then
    		a :: inc1 rest (j-1)
    	else if j < 0 then
    	    a::rest
    	else
    		(a+1) :: rest

let rec inc2 area i j = match area with
    | [] -> []
	| r::rest ->
    	if i > 0 then
    		r :: inc2 rest (i-1) j
    	else if i < 0 then
    	    r::rest
    	else
    		inc1 r j :: rest

let flash1 oct =
	let flashed = ref [] in
	let o2 = ref oct in
	let loop = ref true in
	while !loop do
		let n = List.length !flashed in
		for i = 0 to List.length !o2 - 1 do
			for j = 0 to List.length (List.nth !o2 i) - 1 do
				if (List.nth (List.nth !o2 i) j) > 9 && not (List.mem (i,j) !flashed) then
					(flashed := (i,j) :: !flashed;
					o2 := inc2 !o2 (i-1) (j-1);
					o2 := inc2 !o2 (i) (j-1);
					o2 := inc2 !o2 (i+1) (j-1);
					o2 := inc2 !o2 (i-1) (j);
					o2 := inc2 !o2 (i+1) (j); 
					o2 := inc2 !o2 (i-1) (j+1);
					o2 := inc2 !o2 (i) (j+1);
					o2 := inc2 !o2 (i+1) (j+1))
				else ()
			done
		done;
		loop := (List.length !flashed) > n;
	done;
	!o2

let flash2 oct = (List.length (List.filter (fun x -> x > 9) (List.concat oct)), 
					List.map (List.map (fun x-> if x > 9 then 0 else x)) oct)


let rec count oct n = 
	if n = 0 then 
		0
	else
        let (k, oct2) = flash2(flash1(inc(oct))) in
		k + count oct2 (n-1);;

Printf.printf "%d\n" (count inp 100)

let rec count2 oct n = 
	if n = 0 then 
		0
	else
        let (k, oct2) = flash2(flash1(inc(oct))) in
		if k = 100 then
			1000-n+1
		else
			count2 oct2 (n-1);;

Printf.printf "%d\n" (count2 inp 1000)
