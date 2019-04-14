type tree = L | N of int * tree * tree;;


let tree = (N (15,(N (7,(N (3,L,L)),(N (10,L,L)))),(N (23,(N (18,L,L)),(N (27,(N (25,L,L)),L))))));;

(*          15	     	               
           /   \
	  /     \
         /       \	
        7        23
      /   \    /    \
     3    10  18    27
    /\    /\  /\   /  \
   L  L  L  L L L 25   L
		  /\
		 L  L


*)
let fst (N (a,b,c)) = a;;
let snd (N (a,b,c)) = b;;

let rec delete_elem tr x = 
	match tr with
 L -> L
|N (key,left,right) -> if x = key then (N (,,right)) else (delete_elem right x);;