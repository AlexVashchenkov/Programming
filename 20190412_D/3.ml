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

let rec _end_ l n = 
	if n = (length l) then [] else (get_elem l n) :: (_end_ l (n+1));;
 
let rec split lkp klp = 
	print_string "klp = ";iter (fun x -> print_int x;print_string " ") klp;print_string "; ";
	print_string "lkp = ";iter (fun x -> print_int x;print_string " ") lkp;print_string "\n";
	if (length lkp) = 0 && (length klp) = 0 then (N ((root klp),L,L)) else
	(N ((root klp),
	    (split (sub lkp 0 (index lkp (root klp))) (sub klp 0 ((length klp) - 1))),
	    (split (_end_ lkp ((index lkp (root klp)) + 1)) (sub klp 0 ((length klp - 1))))));;

let klp = [1;2;3;4;5;6];;
let lkp = [3;2;4;1;5;6];;

split lkp klp;;