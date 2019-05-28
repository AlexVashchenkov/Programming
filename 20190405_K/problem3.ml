let r = read_int();;

let pth x y = 
    sqrt ((float_of_int(x*x+y*y)));;

let main2 r = 
	let width = r in
	let length = (int_of_float ((float_of_int r) *. 1.5)) in
	for y=width downto -width do
		for x = (-length) to (length) do
			if ((int_of_float (pth x y)) = r) then print_string "* " else print_string " ";
			x = x+1;	
		done;
        print_string "\n";
	done;;

main2 r;;