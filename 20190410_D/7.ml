open String;;
open Array;;

let a = init l1 (fun x -> init l2 (fun y -> ));;
let rec parse tree = 
	match tree with 
 Star -> 
|Brace (t1,t2) -> let (l,_) = parse l in
			let (r,_) = parse r in