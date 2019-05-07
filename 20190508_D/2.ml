open Array;;

type tree = L | N of int * tree * tree;;

let tree = N (100, (N (23,(N (42,L,L)),(N (10,L,L)))), (N (70,(N (777,(N (6,L,L)),(N (14,L,L)))),(N (9,L,L)))));;

let rec g s = 
	match s with
 L -> 0
|N (n,t1,t2) -> 1 + (max (g t1) (g t2));;

let rec depth s x = 
	match s with
 L -> 0
|N (n,t1,t2) -> if n = x then 0 else 1 + (min (depth t1 x) (depth t2 x));;

let rec bfs tree = 
	let rec f s = 
		match s with
	 [] -> []
	|_ -> List.flatten (List.map (fun x -> match x with
				      	       L -> []
				    	      |N (x,_,_) -> [(x,(depth tree x))]) s) @	
		(f (List.flatten (List.map (fun x -> match x with
				      		     L -> []
				     		    |N (_,l,r) -> [l;r]) s)))
in (f [tree]);;

let rec place m x y l n = 
	if n = (String.length l) then m else (place (m.(x).(y) <- (String.make 1 l.[n]);m) x (y+1) l (n+1));;
 

let rec lst l = 
	match l with
 [] -> []
|(x,y) :: q -> x :: (lst q);;

let rec find_str s n1 n2 x =
  if n2 >= (String.length s) then failwith"find_str" else 
	(if (String.sub s n1 n2) = x then n1 else (find_str s (n1+1) (n2) x));;

let str = (String.concat "" (List.map (fun x -> (string_of_int x)) (lst (bfs tree))));;

let matrix tree = init (4 * ((g tree)) + 1) (fun x -> init (String.length str) (fun y -> " "));;

let rec lkp tree = 
	match tree with
 L -> ""
|N (n,t1,t2) -> (lkp t1) ^ (string_of_int n) ^ (lkp t2);;
 
let rec full l = 
	match l with
 [] -> []
|(x,y) :: q -> (x,y,(find_str (lkp tree) 0 ((String.length (string_of_int x))) (string_of_int x))) :: (full q);;
 
let rec put l matrix = 
	match l with
 [] -> matrix
|(x,y,z) :: q -> if y = 0 then (put q (place matrix (0) (z) (string_of_int x) 0)) else (put q (place matrix (4 * (y)) (z) (string_of_int x) 0));;  
	  
let rec lines_even l matrix = 
	let rec lines matrix str level index = 
		if level = 0 then (place matrix (level+1) (index + 1) "+" ((String.length str) - 2)) else 
		(place matrix (level+1) (index + 1) "+" ((String.length str) - 2);
		place matrix (level-1) (index + 1) "+" ((String.length str) - 2))

in 		match l with
	 [] -> matrix
	|(x,y,z) :: q -> (lines_even q (lines matrix (string_of_int x) (4 * y) z));;

iter (fun x -> iter (fun y -> print_string y) x;print_string "\n") (lines_even (full (bfs tree)) (put (full (bfs tree)) (matrix tree)));;