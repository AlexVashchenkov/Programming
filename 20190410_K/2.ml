type tree = Leaf | Node of int * tree * tree;;

let rec max a b = if a > b then a else b;;                                                                

open String;;

let rec depth s = 
	match s with
 Leaf -> 0
|Node (x,t1,t2) -> 1 + (max (depth t1) (depth t2));;

let rec parse s s2 k = 
	match s2 with
 Leaf -> ()
|Node (x,t1,t2) -> if (depth s2) = k then (
	print_int x;
	print_string " ";
	(parse s t1 k);
	(parse s t2 k)
) 
else ((parse s t1 k);(parse s t2 k));;
                                                                             
for i = 0 to (depth (Node (1,Node (3,Leaf,Leaf),Node (5,Node (7,Leaf,Leaf),Node (6,Leaf,Leaf))))) do
	parse (Node (1,Node (3,Leaf,Leaf),Node (5,Node (7,Leaf,Leaf),Node (6,Leaf,Leaf)))) (Node (1,Node (3,Leaf,Leaf),Node (5,Node (7,Leaf,Leaf),Node (6,Leaf,Leaf)))) i;
done;;