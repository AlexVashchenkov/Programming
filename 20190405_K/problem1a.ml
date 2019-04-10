open String;;

let rec make_str n = 
	if n = 0 then "" else " " ^ (make_str (n-1));;

for i = 0 to 4 do
	print_string ((make_str i) ^ "*\n");
done;;
for i = 3 downto 0 do
	print_string ((make_str i) ^ "*\n");
done;;	