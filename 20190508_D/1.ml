type tree = L | N of int * tree * tree;;

let rec bfs tree = 
	match tree with
 [] -> []
|_ -> List.flatten (List.map (fun x -> match x with
				      L -> []
				     |N (x,_,_) -> [x]) tree) @	
      (bfs (List.flatten (List.map (fun x -> match x with
				      L -> []
				     |N (_,l,r) -> [l;r]) tree)));;

let width tree = String.length (String.concat "" (List.map (fun x -> (string_of_int x)) (bfs [tree])));;

