type tree = L | N of int * tree * tree;;

let a = [|1;2;3;4;5;6;7|];;

let rec make_tree a n = 
	if n = (length a) then L else
	if 2*n+1 >= (length a) then (N (a.(n),L,L)) else
	if 2*n+2 >= (length a) then (N (a.(n),L,L))