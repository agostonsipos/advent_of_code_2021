let inp1 = [("start","A");("start","b");("A","c");("A","b");("b","d");("A","end");("b","end")]
let inp2 = [("fs","end");("he","DX");("fs","he");("start","DX");("pj","DX");("end","zg");("zg","sl");("zg","pj");("pj","he");("RW","he");("fs","DX");("pj","RW");("zg","RW");("start","pj");("he","WI");("zg","he");("pj","fs");("start","RW")]
let inp = [("start","kc");("pd","NV");("start","zw");("UI","pd");("HK","end");("UI","kc");("pd","ih");("ih","end");("start","UI");("kc","zw");("end","ks");("MF","mq");("HK","zw");("LF","ks");("HK","kc");("ih","HK");("kc","pd");("ks","pd");("MF","pd");("UI","zw");("ih","NV");("ks","HK");("MF","kc");("zw","NV");("NV","ks")]


let rec traverse curr passed edges =
	if List.mem curr passed then
		0
	else if curr = "end" then
		1
	else
		let x = ref 0 in 
		let passable = (curr.[0] = Char.uppercase_ascii (curr.[0])) in
		for i = 0 to List.length edges - 1 do
			let (a, b) = List.nth edges i in
			if a = curr then
				x := !x + traverse b (if passable then passed else a::passed) edges
			else if b = curr then
				x := !x + traverse a (if passable then passed else b::passed) edges
		done;
		!x

let x = traverse "start" [] inp;;
Printf.printf "%d\n" x

let rec traverse2 curr passed twice edges =
	if curr = "end" then
		1
	else if twice && List.mem curr passed then
		0
	else
		let x = ref 0 in 
		let passable = (curr.[0] = Char.uppercase_ascii (curr.[0])) in
		for i = 0 to List.length edges - 1 do
			let (a, b) = List.nth edges i in
			if a = curr && b <> "start" then
				x := !x + traverse2 b (if passable then passed else a::passed) (twice || List.mem curr passed) edges
			else if b = curr && a <> "start" then
				x := !x + traverse2 a (if passable then passed else b::passed) (twice || List.mem curr passed) edges
		done;
		!x

let x = traverse2 "start" [] false inp;;
Printf.printf "%d\n" x
