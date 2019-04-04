open Array;;

let n = 13;;

let rec swap a y x n = 
	a.(x).(y) <- n;a;;

let a = init n (fun x -> init n (fun y -> " "));;

let rec draw_vert a x y1 y2 = 
	if y1 = y2 then a else (swap a x y1 "*";draw_vert a x (y1+1) y2);;

let rec draw_gor a x1 x2 y = 
	if x1 = x2 then a else (swap a x1 y "*";draw_gor a (x1+1) x2 y);;

let rec draw_line a x1 y1 x2 y2 x = 
	let k = (y2 - y1 / x2 - x1) in
	let b = y1 - k * x1 in
	if x1 = x2 then (swap a x y2 "*";draw_line a x1 y1 x2 y2 (x+1)) else
	(swap a (k * x + b) y1 "*";draw_line a x1 y1 x2 y2 (x+1));;

let draw_gold a = 
	draw_line a 0 (n/2) (n/2) 0 0;;	

iter (fun x -> iter (fun y -> print_string y) x;print_string "\n") (draw_line a 0 (n/2) (n/2) 0 0);;

