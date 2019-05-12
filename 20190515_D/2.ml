open String;;

let s = "197131924682777";;
let l = "277";;

let rec sum str n = 
	if n = (length str) then 0 else (int_of_string (make 1 str.[n])) + (sum str (n+1));;

let rec robin_carp s l n = 
	if n = ((length s) - (length l)) then failwith"" else
	if (sum (sub s n (length l)) 0) = (sum l 0) 
	then (if (sub s n (length l)) = l then n else (robin_carp s l (n+1))) 
	else (robin_carp s l (n+1));;

robin_carp s l 0;;
