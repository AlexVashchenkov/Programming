type tree = L | N of int * tree * tree;;


let tree = (N (15,(N (7,(N (3,L,L)),(N (10,L,L)))),(N (23,(N (18,L,L)),(N (27,(N (25,L,L)),L))))));;

let rec add_elem tr x = 
	match tr with
 L -> (N (x,L,L))
|N (key,left,right) -> if x < key then (N (key,(add_elem left x),right)) else (N (key,left,(add_elem right x)));;