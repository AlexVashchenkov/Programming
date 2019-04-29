type tree = L | N of int * tree * tree;;

open List;;

let rec klp s = 
	match s with
 L -> []
|N (x,t1,t2) -> x :: (klp t1) @ (klp t2);;

let rec lkp s = 
	match s with
 L -> []
|N (x,t1,t2) -> (lkp t1) @ [x] @ (lkp t2);;

let rec get_elem l n = 
	match l with
 [] -> failwith"get_elem"
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

let rec check l n = 
	if n = ((length l) - 1) then true else
	if (get_elem l (n-1)) <= (get_elem l n) then (check l (n+1)) else false;;

check (lkp (N (15,(N (7,(N (3,L,L)),(N (10,L,L)))),(N (23,(N (18,L,L)),(N (27,(N (25,L,L)),L))))))) 1;;
