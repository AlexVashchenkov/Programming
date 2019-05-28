open Array;;

let n = 11;;

let a = init n (fun x -> init n (fun y -> " "));;

let put a x y = a.(y).(x) <- "*";a;;

let rec draw_line s x y a b c = 
	if y = n then s else
	if x = n then (draw_line s 0 (y+1) a b c) else 
	if a*x+b*y+c=0 then (draw_line (put s x y) (x+1) y a b c) else (draw_line s (x+1) y a b c);; 

let rec draw_int s x y a b c p q = 
	if y = n then a else
	if x = p then (draw_int s q (y+1) a b c p q) else
	if a*x+b*y+c=0 then (draw_int (put s x y) (x+1) y a b c p q) else (draw_int s (x+1) y a b c p q);; 

let rec draw_list s x y l = 
	match l with
 [] -> s
|a :: b -> if x = n then (draw_list s 0 (y+1) l) else (put s x y;draw_list s (x+1) y b);;

let rec create n = 
	if n = 0 then [] else "*" :: (create (n-1));;
 
draw_line a 0 0 1 (-1) (-(n-1)/2);;
draw_line a 0 0 1 1 (-(n-1)/2);;
draw_line a 0 0 0 1 (-(n-1));;
draw_line a 0 0 1 0 (-n/2);;
draw_list a ((n - (List.length (create (n-4)))) / 2) (n-3) (create (n-4));;	

iter (fun x -> iter (fun y -> print_string (y ^ " ")) x;print_string "\n") a;;
