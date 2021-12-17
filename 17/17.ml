let xlim = (282, 314)
let ylim = (-80,-45)

let part1 = let y = - fst ylim - 1 in y*(y+1)/2;;

Printf.printf "%d\n" part1
	
let sim ivx ivy =
	let x = ref 0 in
	let y = ref 0 in
	let vx = ref ivx in
	let vy = ref ivy in
	let (x1,x2) = xlim in
	let (y1,y2) = ylim in
	while (!y > y2 || !x < x1) && (!y > y1 && !x < x2) do
		x := !x + !vx;
		y := !y + !vy;
		vy := !vy - 1;
		if !vx > 0 then vx := !vx -1;
	done;
	!y >= y1 && !y <= y2 && !x >= x1 && !x <= x2;;

let count = 
    let c = ref 0 in
    for i = 24 to 314 do
        for j = -80 to 79 do
            if sim i j then
                c := !c + 1;
        done;
    done;
    !c;;

Printf.printf "%d\n" count
