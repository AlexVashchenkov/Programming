open String;;

type tree = L | N of int * tree * tree;;

let n = 9;;

let daddy = n * n;;

let rec find n x i = 
	if x + i = n then i else (find n (x+i) (i+2));;

let rec string s depth = 
	sub s 0 ((length s) / depth);;

let rec parse a depth length = 
	match a with 
 L -> if depth = 0 then print_string ((make ((length * depth)) ' ') ^ "+-L\n") else print_string ((make ((length * depth) - 1) ' ') ^ "+-L\n")
|N (n,t1,t2) -> print_string ((make (length * depth) ' ') ^ "+-" ^ (string_of_int n) ^ "\n");
		print_string ((make ((length * (depth+1)) - 1) ' ') ^ "|\n");
			(parse t1 (depth + 1) length);
			(parse t2 (depth + 1) length);;

let rec create_tree n k = 
	if k < 0 then L else (N (n,(create_tree (n-k) (k-2)),(create_tree (n+k) (k-2))));;

parse (create_tree daddy (find daddy 0 1)) 0 4;;	