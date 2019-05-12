open Array;;
open String;;

let rec prikl str substr n = 
	if n = (length substr) then [] else
        (Printf.printf "%s%s \n%s%s \n\n" (make (n+1) ' ') (sub str n ((length substr) - n - 1)) (make (n+1) ' ') (sub substr 0 ((length str) - n - 1));
        if (sub str n ((length substr) - n - 1)) = (sub substr 0 ((length str) - n - 1)) then n :: (prikl str substr (n+1)) else (prikl str substr (n+1)));;

let rec max l = 
	let rec maximum l x = 
		match l with
	 [] -> x
	|a :: b -> if a > x then (maximum l a) else (maximum l x)
in (maximum l (List.hd l));;

let next_sost = (max (prikl str substr 0));;
 
prikl "xyzxyz" "ayzxyz" 0;;