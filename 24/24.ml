(* This program does not calculate the answer, which I reached with "pen and paper".
It only checks the result by executing the instruction sequence.*)

type var = W | X | Y | Z | V of int

type statement = Inp of var | Add of var * var | Mul of var * var | Div of var * var | Mod of var * var | Eql of var * var

let w = ref 0
let x = ref 0
let y = ref 0
let z = ref 0
let i = ref 0

let getval = function
	| W -> !w
	| X -> !x
	| Y -> !y
	| Z -> !z
	| V a -> a

let setval name value = match name with
	| W -> w := value
	| X -> x := value
	| Y -> y := value
	| Z -> z := value

let execute statement inp = match statement with
	| Inp a -> setval a inp.(!i); i := !i + 1; Printf.printf "x=%d\ty=%d\tz=%d\n" !x !y !z;
	| Add (a,b) -> setval a (getval a + getval b)
	| Mul (a,b) -> setval a (getval a * getval b)
	| Div (a,b) -> setval a (getval a / getval b)
	| Mod (a,b) -> setval a ((getval a) mod (getval b))
	| Eql (a,b) -> setval a (if getval a = getval b then 1 else 0)

let program = [Inp W;Mul (X,V (0));Add (X,Z);Mod (X,V (26));Div (Z,V (1));Add (X,V (14));Eql (X,W);Eql (X,V (0));Mul (Y,V (0));Add (Y,V (25));Mul (Y,X);Add (Y,V (1));Mul (Z,Y);Mul (Y,V (0));Add (Y,W);Add (Y,V (12));Mul (Y,X);Add (Z,Y);Inp W;Mul (X,V (0));Add (X,Z);Mod (X,V (26));Div (Z,V (1));Add (X,V (13));Eql (X,W);Eql (X,V (0));Mul (Y,V (0));Add (Y,V (25));Mul (Y,X);Add (Y,V (1));Mul (Z,Y);Mul (Y,V (0));Add (Y,W);Add (Y,V (6));Mul (Y,X);Add (Z,Y);Inp W;Mul (X,V (0));Add (X,Z);Mod (X,V (26));Div (Z,V (1));Add (X,V (12));Eql (X,W);Eql (X,V (0));Mul (Y,V (0));Add (Y,V (25));Mul (Y,X);Add (Y,V (1));Mul (Z,Y);Mul (Y,V (0));Add (Y,W);Add (Y,V (4));Mul (Y,X);Add (Z,Y);Inp W;Mul (X,V (0));Add (X,Z);Mod (X,V (26));Div (Z,V (1));Add (X,V (14));Eql (X,W);Eql (X,V (0));Mul (Y,V (0));Add (Y,V (25));Mul (Y,X);Add (Y,V (1));Mul (Z,Y);Mul (Y,V (0));Add (Y,W);Add (Y,V (5));Mul (Y,X);Add (Z,Y);Inp W;Mul (X,V (0));Add (X,Z);Mod (X,V (26));Div (Z,V (1));Add (X,V (13));Eql (X,W);Eql (X,V (0));Mul (Y,V (0));Add (Y,V (25));Mul (Y,X);Add (Y,V (1));Mul (Z,Y);Mul (Y,V (0));Add (Y,W);Add (Y,V (0));Mul (Y,X);Add (Z,Y);Inp W;Mul (X,V (0));Add (X,Z);Mod (X,V (26));Div (Z,V (26));Add (X,V (-7));Eql (X,W);Eql (X,V (0));Mul (Y,V (0));Add (Y,V (25));Mul (Y,X);Add (Y,V (1));Mul (Z,Y);Mul (Y,V (0));Add (Y,W);Add (Y,V (4));Mul (Y,X);Add (Z,Y);Inp W;Mul (X,V (0));Add (X,Z);Mod (X,V (26));Div (Z,V (26));Add (X,V (-13));Eql (X,W);Eql (X,V (0));Mul (Y,V (0));Add (Y,V (25));Mul (Y,X);Add (Y,V (1));Mul (Z,Y);Mul (Y,V (0));Add (Y,W);Add (Y,V (15));Mul (Y,X);Add (Z,Y);Inp W;Mul (X,V (0));Add (X,Z);Mod (X,V (26));Div (Z,V (1));Add (X,V (10));Eql (X,W);Eql (X,V (0));Mul (Y,V (0));Add (Y,V (25));Mul (Y,X);Add (Y,V (1));Mul (Z,Y);Mul (Y,V (0));Add (Y,W);Add (Y,V (14));Mul (Y,X);Add (Z,Y);Inp W;Mul (X,V (0));Add (X,Z);Mod (X,V (26));Div (Z,V (26));Add (X,V (-7));Eql (X,W);Eql (X,V (0));Mul (Y,V (0));Add (Y,V (25));Mul (Y,X);Add (Y,V (1));Mul (Z,Y);Mul (Y,V (0));Add (Y,W);Add (Y,V (6));Mul (Y,X);Add (Z,Y);Inp W;Mul (X,V (0));Add (X,Z);Mod (X,V (26));Div (Z,V (1));Add (X,V (11));Eql (X,W);Eql (X,V (0));Mul (Y,V (0));Add (Y,V (25));Mul (Y,X);Add (Y,V (1));Mul (Z,Y);Mul (Y,V (0));Add (Y,W);Add (Y,V (14));Mul (Y,X);Add (Z,Y);Inp W;Mul (X,V (0));Add (X,Z);Mod (X,V (26));Div (Z,V (26));Add (X,V (-9));Eql (X,W);Eql (X,V (0));Mul (Y,V (0));Add (Y,V (25));Mul (Y,X);Add (Y,V (1));Mul (Z,Y);Mul (Y,V (0));Add (Y,W);Add (Y,V (8));Mul (Y,X);Add (Z,Y);Inp W;Mul (X,V (0));Add (X,Z);Mod (X,V (26));Div (Z,V (26));Add (X,V (-2));Eql (X,W);Eql (X,V (0));Mul (Y,V (0));Add (Y,V (25));Mul (Y,X);Add (Y,V (1));Mul (Z,Y);Mul (Y,V (0));Add (Y,W);Add (Y,V (5));Mul (Y,X);Add (Z,Y);Inp W;Mul (X,V (0));Add (X,Z);Mod (X,V (26));Div (Z,V (26));Add (X,V (-9));Eql (X,W);Eql (X,V (0));Mul (Y,V (0));Add (Y,V (25));Mul (Y,X);Add (Y,V (1));Mul (Z,Y);Mul (Y,V (0));Add (Y,W);Add (Y,V (14));Mul (Y,X);Add (Z,Y);Inp W;Mul (X,V (0));Add (X,Z);Mod (X,V (26));Div (Z,V (26));Add (X,V (-14));Eql (X,W);Eql (X,V (0));Mul (Y,V (0));Add (Y,V (25));Mul (Y,X);Add (Y,V (1));Mul (Z,Y);Mul (Y,V (0));Add (Y,W);Add (Y,V (4));Mul (Y,X);Add (Z,Y);]

let rec run prog inp = match prog with
	| [] -> Printf.printf "x=%d\ty=%d\tz=%d\n" !x !y !z; !z;
	| p :: rest -> execute p inp; run rest inp

let toDec inp = let x = ref 0 in for i = 0 to 13 do x := !x * 10 + inp.(i); done; !x

let check = 
	i := 0;
	let res1 = [|9;9;7;9;9;2;1;2;9;4;9;9;6;7|] in
	if run program res1 = 0 then Printf.printf "%d\n" (toDec res1);
	i := 0;
	let res2 = [|3;4;1;9;8;1;1;1;8;1;6;3;1;1|] in
	if run program res2 = 0 then Printf.printf "%d\n" (toDec res2);

