open Array;; 

let rec check_heap arr = 
	let rec check_heap_down arr n = 
		if n = (length arr) then true else
		(if (2*n+1) >= (length arr) then (check_heap_down arr (n+1)) else
		(if (2*n+2) >= (length arr) then 
			(if 2*n+1 >= (length arr) then (check_heap_down arr (n+1)) else 
		(if arr.(n) >= arr.(2*n+1) then (check_heap_down arr (n+1)) else false)) else
		(if arr.(n) >= arr.(2*n+2) && arr.(n) >= arr.(2*n+1) then (check_heap_down arr (n+1)) else false)))
	and check_heap_up arr n = 
		if n = (length arr) then true else
		(if (2*n+1) >= (length arr) then (check_heap_up arr (n+1)) else
		(if (2*n+2) >= (length arr) then 
			(if 2*n+1 >= (length arr) then (check_heap_up arr (n+1)) else 
		(if arr.(n) <= arr.(2*n+1) then (check_heap_up arr (n+1)) else false)) else
		(if arr.(n) <= arr.(2*n+2) && arr.(n) <= arr.(2*n+1) then (check_heap_up arr (n+1)) else false)))

in Printf.printf "From up to down - %b \nFrom up to down - %b" (check_heap_up arr 0) (check_heap_down arr 0);;

let rec all_less l x = 
	match x with
 [] -> true
|a :: b -> if a < x then (all_less b x) else false;;

let rec left_tree arr n = 
	if 2 * n + 1 >= (length arr) then [] else [arr.(n);arr.(n+1)] @ (left_tree arr (2 * n + 1))

let rec right_tree arr n = 
	if 2 * n + 2 >= (length arr) then [] else [arr.(n);arr.(n+1)] @ (right_tree arr (2 * n + 2))

		 
	 
		 
		