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
	match l with
 [] -> failwith""
|a :: b -> if n1 = n2 then [] else ((sub b (n1+1) n2) @ [a]);;

let rec del l x p = 
	match l with
 a :: b when a = x -> (p,b)
|a :: b -> (del b x (p @ [a]))
|[] -> failwith"";;
 
let rec build_tree klp lkp  = 
	let l1 = (fst (del lkp (root klp) [])) in
	let r1 = (snd (del lkp (root klp) [])) in
	let l2 = (sub klp 1 (length l1)) in
	let r2 = (sub klp (index klp (hd r1)) (length klp)) in
	if (length l1) = 0 && (length r1) = 0 then L else
	(N ((root klp),(build_tree l1 l2),(build_tree r1 r2)));;
	

let klp = [3;7;5;2;8;9;6];;
let lkp = [5;7;8;2;9;3;6];;

build_tree klp lkp;;
