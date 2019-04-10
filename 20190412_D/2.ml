type tree = L | N of int * tree * tree;;

let rec depth s = 
	match s with
 L -> 0
|N (x,t1,t2) -> 1 + (max (depth t1) (depth t2));;

let rec print s k = 
	match s with
 L -> ()
|N (x,t1,t2) -> if ((depth t1)) = k then (print_int x;(print t1 k)) else (print t1 k);
		if ((depth t2)) = k then (print_int x;(print t2 k)) else (print t1 k);;

print (N (1,(N (3,L,L)),(N (5,(N (7,L,L)),(N (6,L,L)))))) 3;;