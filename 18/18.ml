type tree = Leaf of int | Node of tree * tree

let rec parse str pos = 
	let ch = str.[pos] in
	if ch = '[' then
		let (x, a) = parse str (pos+1) in
		let (y, b) = parse str a in
		(Node (x, y), b)
	else if ch = ',' || ch = ']' then
		parse str (pos+1)
	else
		let n = (Char.code ch) - (Char.code '0') in
		(Leaf n, pos + 1);;

let rec print tr = match tr with
    | Leaf a -> Printf.printf "%d" a
    | Node (x, y) -> Printf.printf "["; print x; Printf.printf ","; print y; Printf.printf "]"

let rec inc tr a left = match tr with
		| Leaf x -> Leaf (x+a)
		| Node (x,y) -> if left then Node (inc x a left, y) else Node (x, inc y a left)

let rec explode tr n = match tr with
	| Leaf a -> (false, tr, 0, 0)
	| Node (Leaf a, Leaf b) ->
		if n <= 0 then
			(true, Leaf 0, a, b)
		else
			(false, tr, 0, 0)
	| Node (x, y) ->
		let (bang, res, a, b) = explode x (n-1) in
		if bang then
			if b <> 0 then
				let r = inc y b true in
				(true, Node (res, r), a, 0)
			else
				(true, Node (res, y), a, b)
		else
			let (bang, res, a, b) = explode y (n-1) in
			if bang then
				if a <> 0 then
					let l = inc x a false in
					(true, Node (l, res), 0, b)
				else
					(true, Node (x, res), a, b)
			else
				(false, tr, 0, 0)

let rec split tr = match tr with
    | Leaf a -> if a >= 10 then (Node (Leaf (a/2), Leaf (a-a/2)), true) else (Leaf a, false)
    | Node (x, y) -> 
        let (res, splat) = split x in
        if splat then 
            (Node (res, y), splat) 
        else 
            let (res, splat) = split y in
            (Node (x, res), splat)

let reduce tree = 
    let loop = ref true in
    let tr = ref tree in
    while !loop do
        let loop2 = ref true in
        while !loop2 do
            let (b, t, _, _) = explode !tr 4 in
            (tr := t; loop2 := b)
        done;
        let (t, b) = split !tr in
        (tr := t; loop := !loop2 || b)
    done;
	!tr;;

let add tr1 tr2 = reduce (Node (tr1, tr2));;

let rec magnitude tr = match tr with
	| Leaf a -> a
	| Node (x, y) -> 3*magnitude x + 2*magnitude y;;


let inp = ["[[[2,[3,5]],[8,7]],[[9,3],2]]";"[[3,[3,7]],[[3,6],[[1,1],7]]]";"[8,[[5,5],[2,9]]]";"[[5,[3,5]],[[2,1],[[7,1],[7,7]]]]";"[[[[3,3],0],[[0,3],0]],[[8,[2,2]],[[0,4],3]]]";"[3,6]";"[[5,[[4,2],1]],[[6,[0,3]],[4,[7,7]]]]";"[[6,5],[2,[3,6]]]";"[[[[0,1],0],[[7,4],5]],[[6,2],[4,[0,8]]]]";"[[[[4,7],3],8],[[7,[0,4]],[7,[1,4]]]]";"[[[0,[9,8]],[2,9]],[[[6,4],[4,0]],4]]";"[2,[[4,[8,5]],[6,8]]]";"[[[0,7],[5,[3,0]]],[[[6,4],[3,2]],[[4,7],[9,6]]]]";"[[[[0,6],[0,7]],[8,0]],[8,[4,8]]]";"[[[[9,9],2],[[6,2],[2,2]]],[[5,[8,8]],6]]";"[[0,[[4,6],7]],[[7,[4,8]],9]]";"[[0,5],[[5,3],[[3,9],4]]]";"[2,[[[9,4],[8,8]],1]]";"[5,[[[2,3],6],[2,[7,0]]]]";"[[7,[[8,6],3]],[2,[2,7]]]";"[6,[[2,4],[[9,7],[5,9]]]]";"[[[9,[2,1]],9],1]";"[[[6,9],[2,[2,5]]],[[[4,4],0],7]]";"[1,[[[3,9],[6,1]],[4,0]]]";"[[[3,8],[3,[2,7]]],[[[9,2],2],6]]";"[6,[[8,[3,1]],7]]";"[[[9,9],7],[[[3,1],[8,4]],[0,0]]]";"[[[1,[7,8]],[4,2]],2]";"[[9,7],6]";"[[6,[4,8]],[[[8,6],[0,1]],[[0,4],[8,4]]]]";"[[[[1,8],[8,6]],[9,[2,0]]],[5,[2,[7,2]]]]";"[1,9]";"[[8,[9,[9,3]]],[[[1,1],8],[[1,5],[8,6]]]]";"[[[3,[4,4]],3],[[7,0],[6,0]]]";"[[[6,[6,3]],[6,7]],[1,[8,0]]]";"[[[9,7],[1,7]],8]";"[[8,[[4,6],[4,8]]],8]";"[[[1,9],6],1]";"[[[[0,5],[0,0]],7],[4,8]]";"[[[[6,0],[4,2]],[8,[5,1]]],[[0,[4,8]],[[3,2],8]]]";"[[[[5,9],[5,8]],[9,[0,1]]],[[[8,6],[3,1]],[[9,8],0]]]";"[0,[[9,9],[6,2]]]";"[[[[7,9],[9,1]],[[1,0],[6,4]]],[4,[[2,1],2]]]";"[4,2]";"[[[6,5],[[0,6],2]],[[[1,2],0],[[8,9],8]]]";"[[8,[[4,1],0]],[[[1,5],[3,5]],3]]";"[[[8,3],[[9,1],[8,1]]],[[9,9],3]]";"[[2,7],[[[3,9],[2,3]],9]]";"[[2,[[7,3],[1,6]]],[[4,4],[2,7]]]";"[[[5,6],[3,[5,3]]],[[[2,8],0],[4,[8,8]]]]";"[[[1,2],[4,[5,8]]],[8,[8,[9,0]]]]";"[[[[0,5],[8,1]],0],[[[5,4],[6,9]],[[7,5],[4,9]]]]";"[[9,[2,1]],[[[3,8],[9,5]],[[4,4],4]]]";"[[[5,9],[[1,1],[8,9]]],[[1,9],8]]";"[[[8,8],[3,9]],[[[2,1],0],9]]";"[[[[7,8],2],[5,[3,9]]],[6,1]]";"[[[[2,4],[9,1]],[[9,8],[4,4]]],[0,1]]";"[[[[8,8],0],9],4]";"[[[8,[1,5]],0],[[[8,5],4],[[7,3],[9,5]]]]";"[[[5,4],[[5,1],2]],[[[6,8],6],[[3,6],[1,9]]]]";"[[[3,[2,5]],[6,[6,2]]],[[0,7],[3,9]]]";"[3,[[2,9],8]]";"[[[[3,7],[1,6]],[[9,9],[0,3]]],[[[7,3],8],[[3,1],6]]]";"[[[[7,1],4],[[4,0],[4,5]]],[8,[[5,3],[4,6]]]]";"[[[[0,8],1],[7,9]],[[7,5],[[1,0],[0,9]]]]";"[[[9,7],[0,[7,8]]],2]";"[[[5,2],5],[0,[[1,6],[2,0]]]]";"[[[[3,9],7],7],[[3,[3,4]],[0,[5,9]]]]";"[[[[2,5],[9,9]],[1,[6,5]]],6]";"[[[1,[5,9]],[[1,1],1]],[5,[[0,4],[9,0]]]]";"[[[5,8],[0,7]],[3,[2,[8,6]]]]";"[[[[0,7],[7,9]],[[8,4],[8,7]]],[0,[[3,7],9]]]";"[[[5,[5,5]],[[9,5],8]],[[[2,1],5],9]]";"[5,[4,[[3,6],[3,2]]]]";"[[[9,4],3],[[[8,7],[7,5]],[8,[7,7]]]]";"[9,[[[9,2],0],[[9,9],[4,3]]]]";"[[[4,[7,2]],[[7,9],[5,4]]],1]";"[[[[4,9],5],7],[[5,6],0]]";"[[[5,[3,1]],[8,1]],[8,[7,0]]]";"[[5,6],[6,[[0,5],0]]]";"[[[5,[4,5]],9],6]";"[[[9,[7,0]],6],[2,[1,6]]]";"[[[9,[8,4]],[7,[6,0]]],[[[4,6],[7,5]],[8,[0,8]]]]";"[0,7]";"[[3,[3,8]],[9,[[3,1],[4,4]]]]";"[[6,7],[8,9]]";"[[[[9,8],[0,2]],[[4,0],[7,5]]],[[[5,0],1],2]]";"[[[[1,2],[3,9]],1],[[5,1],[0,1]]]";"[[[[5,8],0],6],[7,0]]";"[[[8,[5,4]],[[3,0],7]],[[8,[7,5]],4]]";"[[[[5,8],8],8],[[[0,4],[2,5]],0]]";"[[[9,6],3],[[[3,3],1],[2,[9,2]]]]";"[[[6,3],6],[[[4,1],8],[2,3]]]";"[2,[[1,8],0]]";"[5,[[[7,6],[1,9]],[4,[8,2]]]]";"[[[[6,9],[0,7]],[[2,7],8]],[[6,0],[2,[1,6]]]]";"[[[[7,8],[5,1]],[[2,9],2]],0]";"[5,3]";"[2,[7,[7,[5,8]]]]";"[[3,3],[8,[2,6]]]"]

let sum = function
	| a :: rest -> List.fold_left add (fst (parse a 0)) (List.map (fun x -> fst (parse x 0)) rest);;

Printf.printf "%d\n" (magnitude (sum inp))

let find2bigsum inp =
    let n = List.length inp in
	let max = ref 0 in
	for i = 0 to n-1 do
		for j = 0 to n-1 do
			if i <> j then
				let m = magnitude (add (List.nth inp i) (List.nth inp j)) in
				if m > !max then max := m
		done;
	done;
	!max

let m = find2bigsum (List.map (fun x -> fst (parse x 0)) inp);;
Printf.printf "%d\n" m
