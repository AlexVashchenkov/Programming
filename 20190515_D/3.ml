open String;;

let rec hash str n = 
	if n = (length str) then 0 else (int_of_float (11. ** (float_of_int ((length str) - n)))) * (int_of_char str.[n]) + (hash str (n+1));;

let rec random_str n = 
	if n = 0 then "" else (make 1 (Char.chr (Random.int 255))) ^ (random_str (n-1));;

let rec robin_carp s l n = 
	if n = ((length s) - (length l)) then failwith"" else
	if (hash (sub s n (length l)) 0) = (hash l 0) 
	then (if (sub s n (length l)) = l then n else (robin_carp s l (n+1))) 
	else (robin_carp s l (n+1));;

let rec check n k = 
	if n > k then () else
	(let x = Sys.time() in
	 (robin_carp (random_str n) (random_str 10) 0);
	 let y = Sys.time() in
	 (print_int n;print_string " ";print_float (y-.x);(check (n * 2) k)));;

