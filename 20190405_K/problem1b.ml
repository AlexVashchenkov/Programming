open String;;

let rec make_str n = 
	if n = 0 then "" else " " ^ (make_str (n-1));;

let a = read_int();;

for i = 0 to a do
	print_string ((make_str i) ^ "*\n");
done;;
for i = (a-1) downto 0 do
	print_string ((make_str i) ^ "*\n");
done;;	