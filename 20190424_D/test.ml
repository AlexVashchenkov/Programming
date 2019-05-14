open String;;

type tree = L | N of int * tree * tree;;

let rec draw t s = 
	match t with
 N (i,t1,t2) -> print_string (s ^ "+--" ^ (string_of_int i) ^ "\n");
		(draw t1 (s ^ "|  "));
		(draw t2 (s ^ "   "))
|L -> print_string (s ^ "+--L");;