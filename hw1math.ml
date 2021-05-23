(*2.*)
type expr =
	| Const of int
	| Var of string
	| Plus of arg
	| Mult of arg
	| Minus of arg
	| Div of arg
and arg =
	{ arg1 : expr;
		arg2 : expr;
	}
;;

(*3.*)
let rec evaluate e = match e with
	| Const c -> c
	| Var v -> 0 
	| Plus {arg1 = a1; arg2 = a2} -> (evaluate a1) + (evaluate a2)
	| Mult {arg1 = a1; arg2 = a2} -> (evaluate a1) * (evaluate a2)
	| Minus {arg1 = a1; arg2 = a2} -> (evaluate a1) - (evaluate a2)
	| Div {arg1 = a1; arg2 = a2} -> (evaluate a1) / (evaluate a2)
;;
