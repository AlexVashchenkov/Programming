open String;;

type tree = L | N of int * tree * tree;;

let tree = N (1,(N (2,(N (3,L,L)),(N (4,L,L)))),(N (5,(N (6,L,L)),(N (7,L,L)))));;

let rec find tree k = 
	match tree with
 L -> k-1
|N (n,t1,t2) -> if n > k then n else ((find t1 k);(find t2 k));;