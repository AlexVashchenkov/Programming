open Array;;

let rec swap a n1 n2 = 
	let t = a.(n1) in a.(n1) <- a.(n2)
let rec reverse_heap arr n = 
	if n = (length arr) then arr else
	if 	