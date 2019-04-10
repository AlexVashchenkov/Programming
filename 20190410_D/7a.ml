open String;;

type comma = Brace of comma * comma | Star;;

type tree = Star | Comma of tree * tree;;

let parse_ s = 
	let rec parse_comma p = 
		match s.[p] with
	 '*' -> (Star, p+1)
	|'(' -> let (left,p_) = parse_comma (p+1) in
		if s.[p_] <> ',' then (Printf.printf "',' expected on symbol %d \n" p_);
		let (right,p__) = parse_comma (p_+1) in
		if s.[p__] <> ')' then (Printf.printf "')' expected on symbol %d \n" p__);
		(Comma (left,right), p__ + 1)
	|_ -> (failwith "Unexpected symbol")
	
	in (let (t,p) = (parse_comma 0) in (if p <> String.length s then failwith "Extra symbols" else t));;

let rec parse a depth = 
	match a with
 Star -> print_string ("|\n+--*\n")
|Brace (x,y) -> print_string "|\n+-()\n";(parse x (depth+1));(parse y (depth + 1));;

parse (parse_ (read_line())) 0;;