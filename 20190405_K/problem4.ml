open Array;;

let n = read_int();;

let a = init n (fun x -> init n (fun y -> " "));;

let put a x y = a.(y).(x) <- "*";a;;

let rec draw_line s x y a b c = 
	if y = n then s else
	if x = n then (draw_line s 0 (y+1) a b c) else 
	if a*x+b*y+c=0 then (draw_line (put s x y) (x+1) y a b c) else (draw_line s (x+1) y a b c);; 
	
draw_line a 0 0 1 0 (-(n/2));;
draw_line a 0 0 0 1 (-(n/2));;
draw_line a 0 0 0 1 0;;
draw_line a 0 0 0 1 (-n+1);;

iter (fun x -> iter (fun y -> print_string (y ^ " ")) x;print_string "\n") a;;
