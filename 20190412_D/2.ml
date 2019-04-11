type tree = L | N of int * tree * tree;;

let rec depth s = 
	match s with
 L -> 0
|N (x,t1,t2) -> 1 + (max (depth t1) (depth t2));;

let rec print s d k = 
	match s with
 L -> ()
|N (x,t1,t2) -> if d = k then (print_int x;(print t1 (d+1) k);(print t2 (d+1) k)) else ((print t1 (d+1) k);(print t2 (d+1) k));;

for i = 0 to (depth (N (1,(N (3,L,L)),(N (5,(N (7,L,L)),(N (6,L,L))))))) do		
	print (N (1,(N (3,L,L)),(N (5,(N (7,L,L)),(N (6,L,L)))))) 0 i;
done;;
