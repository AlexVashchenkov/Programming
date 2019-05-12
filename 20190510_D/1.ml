open Graphics;;
open_graph"600x600";;
set_text_size 60;;

let rec number l x = 
	match l with
 [] -> failwith""
|a :: b -> if a = x then 1 else 1 + (number b x);;

let rec draw_list lst heigth = 
	match lst with
 [] -> ()
|a :: b -> moveto (50 + 100 * (List.length lst - (number lst a))) heigth;
	   draw_circle (50 + 100 * (List.length lst - (number lst a))) heigth 50;
	   draw_string (string_of_int a);
	   (draw_list b heigth);;

let rec draw_lines lst1 lst2 = 
	match lst1 with
 [] -> ()
|a :: b -> moveto ((-50 + 100 * ((number (List.rev lst2) a))) + 50 * ((number (List.rev lst1) a))) 350;
	   lineto ((-50 + 100 * ((number (List.rev lst1) a))) + 50 * ((number (List.rev lst2) a))) 250;
	   (draw_lines b lst2);;

draw_list (List.rev [0;1;2;3;4]) 400;;
draw_list (List.rev [0;1;2;3;4]) 200;;
draw_lines [0;1;2;3;4] [1;2;3;4;0];;
let x = read_key();; 
