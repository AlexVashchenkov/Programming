type tree = L | N of int * tree * tree;;

open List;;

let rec root klp =
	List.hd klp;;

let rec get_elem l n = 
	match l with 
 [] -> failwith"get_elem"
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

let rec index l x = 
	match l with
 [] -> failwith"index"
|a :: b -> if a = x then 0 else 1 + (index b x);;

let rec sub l n1 n2 = 
	if n2 >= (length l) || n1 < 0 then failwith"sub" else 
	(if n1 = n2 then [] else (get_elem l n1) :: (sub l (n1+1) n2));;

let rec parse_lkp lkp = 
	if (length (sub lkp 0 (index lkp (root lkp)))) = 1 then L else
	(N ((root lkp),
	    (parse_lkp (sub lkp 0 (index lkp (root lkp)))),
	    (parse_lkp (sub lkp ((index lkp (root lkp)) + 1) ((length lkp) - 1)))));;	 
	
