type tree = Leaf | Node of int * tree * tree;;

let rec print_klp s = 
	match s with
 Leaf -> ()
|Node (x,t1,t2) -> print_int x;print_string " ";(print_klp t1);(print_klp t2);;

let rec print_lkp s = 
	match s with
 Leaf -> ()
|Node (x,t1,t2) -> (print_lkp t1);print_int x;print_string " ";(print_lkp t2);;

print_klp (Node (1,Node (3,Leaf,Leaf),Node (5,Node (7,Leaf,Leaf),Node (6,Leaf,Leaf))));;

print_lkp (Node (1,Node (3,Leaf,Leaf),Node (5,Node (7,Leaf,Leaf),Node (6,Leaf,Leaf))));;
