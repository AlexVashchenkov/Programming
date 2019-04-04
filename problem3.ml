let r = read_int();;

let pth x y = 
    sqrt ((float_of_int(x*x+y*y)));;

let rec main1 r x y = 
	let width = r in
	let length = (int_of_float ((float_of_int r) *. 1.5)) in
	if y <= -width then () else
	if x >= length then (print_string "\n";(main1 r (-length) (y-2))) else
	if (int_of_float (pth x y)) = r then (print_string "*";(main1 r (x+1) y)) else (print_string " ";(main1 r (x+1) y));; 

let main2 r = 
	let width = r in
	let length = (int_of_float ((float_of_int r) *. 1.5)) in
	for y=width downto -width do
		for x = (-length) to (length) do
			if ((int_of_float (pth x y)) = r) then print_string "*" else print_string " ";
			x = x+1;	
		done;
        print_string "\n";
	done;;

main1 r 0 r;;
print_string "\n\n\n";;
main2 r;;