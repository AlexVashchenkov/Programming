type binop= Plus|Mul|Minus|Deg;; 

type expression = Lit of int | Binop of expression * binop * expression;; 

let rec str n = if n = 0 then "" else " " ^ (str (n-1));;

let s = read_line() ^ ";";; 

let rec parse_x str i= 
		let (expr,m0) = parse_s str i in 
			if str.[m0] = '-' then 
				let (expr2, m2) = parse_x str (m0 + 1) in 
					(Binop (expr,Minus,expr2),m2) 
			else 
				(expr,m0) 

	and parse_s str i= 
		let (expr,m) = parse_m str i in 
			if str.[m] = '+' then 
				let (expr2, m2) = parse_s str (m + 1) in 
					(Binop (expr,Plus,expr2),m2) 
			else 
				(expr,m) 
	and parse_m str i= 
		let (expr,i1) = parse_t str i in 
			if str.[i1] = '*' then 
				let (expr2,i2) = parse_m str (i1 + 1) in 
					(Binop (expr,Mul,expr2),i2) 
			else 
				(expr,i1)	
	and parse_t str i= 
		match str.[i] with 
	 '0' -> (Lit 0,(i + 1)) 
	|'1' -> (Lit 1,(i + 1)) 
	|'2' -> (Lit 2,(i + 1)) 
	|'3' -> (Lit 3,(i + 1)) 
	|'4' -> (Lit 4,(i + 1)) 
	|'5' -> (Lit 5,(i + 1)) 
	|'6' -> (Lit 6,(i + 1)) 
	|'7' -> (Lit 7,(i + 1)) 
	|'8' -> (Lit 8,(i + 1)) 
	|'9' -> (Lit 9,(i + 1)) 
	|'(' -> let (expr,i1) = parse_x str (i + 1) in 
		if str.[i1] = ')' then 
			(expr,(i1 + 1)) 
		else 
			failwith ") expected" 
	|_ -> failwith ("Unexpected symbol on index " ^ (string_of_int i));; 

let rec string_of_binop b = if b = Plus then "+" else (if b = Minus then "-" else "*");;

let rec print a depth length = 
	match a with 
 Lit x -> print_string ((str (length * depth)) ^ (string_of_int x) ^ "\n");	
|Binop (a,binop,b) -> print_string ((str (length * depth)) ^ (string_of_binop binop) ^ "\n");
			(print a (depth + 1) length);
			(print b (depth + 1) length);
		print_string ((str (length * depth)) ^ "\n");;

print (fst (parse_x "1*(2+6);" 0)) 0 4;;