let move pos n = let x = (pos+n) mod 10 in if x = 0 then 10 else x

let score start1 start2 =
	let x1 = ref 0 in
	let x2 = ref 0 in
	let pos1 = ref start1 in
	let pos2 = ref start2 in
	let loop = ref true in
	let cnt = ref 0 in
	while !loop do
    let i = !cnt in
		let n1 = move !pos1 (6*i+1+6*i+2+6*i+3) in
		x1 := !x1 + n1;
		pos1 := n1;
		if !x1 >= 1000 then
			(Printf.printf "%d\n" (!x2 * (6*i+3)); loop := false);
		let n2 = move !pos2 (6*i+4+6*i+5+6*i+6) in
		x2 := !x2 + n2;
		pos2 := n2;
		if !x2 >= 1000 then
      (Printf.printf "%d\n" (!x1 * (6*i+6)); loop := false);
    cnt := !cnt + 1
    done;;
			
score 4 8;;
score 10 3;;

let add (a,b) (c,d) = (a+c,b+d)
let mul w (a,b) = (w*a, w*b)

let rec results (pos1, sc1) (pos2, sc2) play1 =
	if sc1 >= 21 then
		(1,0)
	else if sc2 >= 21 then
		(0,1)
	else
		if play1 then
			let nextpos = List.map (move pos1) [3;4;5;6;7;8;9] in
			let weights = [1;3;6;7;6;3;1] in
			let f = fun (n,w) -> mul w (results (n, sc1+n) (pos2, sc2) false) in
			List.fold_left (add) (0,0) (List.map f (List.combine nextpos weights))
		else
			let nextpos = List.map (move pos2) [3;4;5;6;7;8;9] in
			let weights = [1;3;6;7;6;3;1] in
			let f = fun (n,w) -> mul w (results (pos1, sc1) (n, sc2+n) true) in
			List.fold_left (add) (0,0) (List.map f (List.combine nextpos weights))

let (res1,res2) = results (10,0) (3,0) true;;

Printf.printf "%d %d\n" res1 res2
