open String;;

type comma = Brace of comma * comma | Star;;

let rec string s depth = 
	sub s 0 ((length s) / depth);;

let rec parse a depth length = 
	match a with 
 Star -> print_string ((make (length * depth) ' ') ^ "+-#\n")
|Brace (x,y) -> print_string ((make (length * depth) ' ') ^ "+-()\n");
		print_string ((make (length * (depth+1)) ' ') ^ "|\n");
			(parse x (depth + 1) length);
			(parse y (depth + 1) length);;

parse (Brace (Star,(Brace (Star,Star)))) (str 1024) 0 2;;
       