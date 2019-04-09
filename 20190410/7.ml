open String;;

type comma = Brace of comma * comma | Star;;

let rec lines n = if n = 0 then "" else (if n = 1 then "    " else "  |" ^ (lines (n-1)));;

let rec str n = if n = 0 then "" else "   " ^ (str (n-1))

let rec reverse s n = if (length s) mod 2 = 0 then (if n > (((length s) / 2)) then (make 1 s.[(length s) - n - 1]) else (make 1 s.[(length s) - n - 1]) ^ (reverse s (n+1))) else
		(if n > (((length s) / 2) + 1) then (make 1 s.[(length s) - n - 1]) else (make 1 s.[(length s) - n - 1]) ^ (reverse s (n+1)));; 

let rec parse a depth = 
	match a with 
 Star -> print_string ((lines (depth)) ^ "+--#\n")
|Brace (x,y) -> 
		print_string ((reverse (lines (depth)) 0) ^ "+-()\n");
		print_string ((reverse (lines (depth+1)) 0) ^ "|\n");
			(parse x (depth + 1));
			(parse y (depth + 1));;

parse (Brace ((Brace ((Brace (Star,Star)),Star)),Star)) 0;;
       