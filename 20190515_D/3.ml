open String;;

let rec hash str n = 
	if n = (length str) then 0 else (int_of_float (11. ** (float_of_int ((length str) - n)))) * (int_of_char str.[n]) + (hash str (n+1));;
let s = "abcdefghijklmnopqrstuvwxyz";;

let rec random_str n = 
	if n = 0 then "" else (make 1 s.[(Random.int 26)]) ^ (random_str (n-1));;
  
let rec robin_carp s l n = 
	if n = ((length s) - (length l)) then failwith"" else
	if (hash (sub s n (length l)) 0) = (hash l 0) 
	then (if (sub s n (length l)) = l then n else (robin_carp s l (n+1))) 
	else (robin_carp s l (n+1));;

let rec deg_2 n = if n = 0 then 1 else 2 * (deg_2 (n-1));;

let rec check_n n k = 
	if n = k then () else
	let x = Sys.time() in
	(robin_carp (Ra  