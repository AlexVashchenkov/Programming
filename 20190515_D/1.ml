open Array;;
open String;;
(*print_string "Enter the text: ";;
let str = read_line();;
print_string "Enter the substring: ";;
let substr = read_line();;*)

let str = "xyzxyx";;

let rec check_repeat s i j = 
	let len = ((length s) / 2) in 
	if i > len then [j] else (
	if (sub s 0 i) = (sub s ((length s) - i) i) 
		then ((*(print_string ((sub s 0 i) ^ " , " ^ (sub s ((length s) - i) i) ^ " "));
		      Printf.printf "; i = %d" i;
		      Printf.printf "; j = %d" j;		      
		      print_string "\n";*)
		      (check_repeat s (i+1) i))
	        else ((*(print_string ((sub s 0 i) ^ " , " ^ (sub s ((length s) - i) i) ^ " "));                    
		      Printf.printf "; i = %d" i;
		      Printf.printf " ; j = %d" j;
		      print_string "\n";*)
		      j :: (check_repeat s (i+1) 0)));;

let rec maximum l = 
	let rec maximum_ l t = 
		match l with 
	 [] -> t
	|a :: b -> if a > t then (maximum_ b a) else (maximum_ b t)
in (maximum_ l (List.hd l));;

let rec all_repeats s n = 
	if n = (length s) then [] else (maximum (check_repeat (sub s 0 (n+1)) 1 0)) :: (all_repeats s (n+1));;

let max_move s = (maximum (all_repeats s 0));;

let rec is_in l x =
	match l with
 [] -> false
|a::b -> if a = x then true else (is_in b x);; 
	
let rec different s n l = 
	if n = (length s) then l else 
	if (is_in l (make 1 s.[n])) then (different s (n+1) l) else (different s (n+1) ((make 1 s.[n]) :: l));; 

let rec put_list m l level index =
	if index + List.length l > (Array.length m.(level)) then failwith"" else 
	match l with
 [] -> m
|a :: b -> m.(level).(index) <- a;(put_list m b level (index+1));;
 
let matrix = Array.init ((length str) - 1) (fun x -> if x = 0 then Array.init ((List.length (different str 0 []))+2) (fun x -> " ") else (Array.append [|(string_of_int (x-1))|] (Array.init ((List.length (different str 0 []))+1) (fun x -> " "))));;

put_list matrix ((List.rev (different str 0 [])) @ ["*"]) 0 1;;

let rec lst_mmoves str lst = 
	match lst with
 [] -> []
|a :: b -> (max_move (str ^ a)) :: (lst_mmoves str b);;
	
let rec put_sosts str sost index matrix = 
Array.iter (fun x -> Array.iter (fun y -> print_string y; print_string " ") x;print_string "\n") matrix; 
	if sost = (Array.length matrix) then matrix else
	if index = ((List.length (different str 0 []))+2) then (put_sosts str (sost+1) 0 matrix) else
	let substr = (sub str 0 sost) ^ (matrix.(0).(index)) in
	(put_sosts str sost (index+1) (put_list matrix (List.map (fun x -> (string_of_int x)) (lst_mmoves substr ((List.rev (different str 0 [])) @ ["*"]))) sost index));; 

lst_mmoves "x" ["x";"y";"z";"*"];;
lst_mmoves "xy" ["x";"y";"z";"*"];;
lst_mmoves "xyz" ["x";"y";"z";"*"];;
lst_mmoves "xyzx" ["x";"y";"z";"*"];;
lst_mmoves "xyzxy" ["x";"y";"z";"*"];;
lst_mmoves "xyzxyx" ["x";"y";"z";"*"];;

(*put_sosts str 1 1 matrix;;*)	