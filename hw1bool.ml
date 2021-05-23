(*Jeffery Luo 113160868 CSE 216 HW 1*)
(*1*)
type bool_expr =
	| Lit of string
	| Not of bool_expr
	| And of bool_expr * bool_expr
	| Or of bool_expr * bool_expr;;

let rec truth a aval b bval ex = match ex with
	| Lit s -> if s = a then aval else bval
	| Not n -> not(truth a aval b bval n)
	| And(a1, a2) -> truth a aval b bval a1 && truth a aval b bval a2
	| Or(o1, o2) -> truth a aval b bval o1 || truth a aval b bval o2;;

let truth_table a b ex = 
	[(true, true, truth a true b true ex); 
		(true, false, truth a true b false ex); 
		(false, true, truth a false b true ex);
		(false, false, truth a false b false ex)];;