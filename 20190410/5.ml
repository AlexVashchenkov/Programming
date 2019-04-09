type comma = Brace of comma * comma | Star;;

let rec str n = if n = 0 then "" else " " ^ (str (n-1));;

let rec parse a depth length = 
	match a with 
 Star -> print_string ((str (length * depth)) ^ "*\n")
|Brace (x,y) -> print_string ((str (length * depth)) ^ "(\n");
			(parse x (depth + 1) length);
			(parse y (depth + 1) length);
		print_string ((str (length * depth)) ^ ")\n");;

parse (Brace (Star,                                                                 
      (Brace (Star,
      (Brace (Star,
      (Brace (Star,
      (Brace (Star,
      (Brace (Star,
      (Brace (Star,
      (Brace (Star,
      (Brace (Star,
      (Brace (Star,
      (Brace (Star,
      (Brace (Star,Star)))))))))))))))))))))))) 0 4;;
       