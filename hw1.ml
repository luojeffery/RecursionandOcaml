(*1a.*)
let rec pow x n = match n with
        | 0 -> 1
        | _ -> x * pow x (n-1);;

(*1b.*)
let rec float_pow x n = match n with
        | 0 -> 1.0
        | _ -> x *. float_pow x (n-1);;

(*2.*)
let rec compress lst = match lst with
	| h :: (h1 :: t) -> if h = h1 then compress (h1 :: t) else h :: compress (h1 :: t)
	| l -> l;;

(*3.*)
let rec remove_if lst p = match lst with
	| [] -> []
	| h :: t -> if p h then remove_if t p else h :: remove_if t p;;

(*4.*)
let slice lst i j =
	let rec select n list = match list with
		| [] -> []
		| head ::tail -> if i > j then [] else if n = 0 then [] else head :: select (n-1) tail in
		let rec remove n list = match list with
			| [] -> []
			| head ::tail as l -> if n = 0 then l else remove (n-1) tail in
				select (j - i) (remove i lst);;

(*5.*)
let rec equivs_helper_single f cl x = match cl with
	| [] -> [[x]]
	| h :: t -> match h with
		| [] -> []
		| head :: tail -> if (f head x) then [h @ [x]] @ t else h :: (equivs_helper_single f t x);;

let rec equivs_helper_list f cl l = match l with
	| [] -> cl
	| h :: t -> (equivs_helper_list f (equivs_helper_single f cl h) t);;

let equivs f lst = equivs_helper_list f [] lst;;

(*6.*)
let checkPrime n =
    let rec remainder x d = match d with
        | 1 -> true    
        | _ -> (x mod d <> 0) && remainder x (d-1)
    in match n with
	    | 0 | 1 -> false
	    | _ -> remainder n (n-1);;

let goldbachpair n =
	let rec pair x =
		if checkPrime x && checkPrime (n-x) then (x, n-x) else pair (x+1) in
	pair 2;;

(*7.*)
let f i = i * i;;
let g i = 3 * i;;
let rec equiv_on f g lst = match lst with
	| [] -> true
	| h :: t -> if f h = g h then equiv_on f g t else false;;

(*8.*)
let rec pairwisefilter cmp lst = match lst with
	| [] -> []
	| head :: head1 :: tail -> let l = pairwisefilter cmp tail in
		(cmp head head1) :: l
	| last :: [] -> [last];;

(*9.*)
let rec polynomial lst =
	fun n -> match lst with
		| [] -> 0
		| pair :: tail -> let term = (fst pair) * pow n (snd pair) in
			let other = polynomial tail in
			term + other n;;

(*10.*)
let rec append lst x = match lst with
	| [] -> []
	| h :: t -> (h @ [x]) :: (append t x);;

let rec powerset lst = match lst with
	| [] -> [[]]
	| h :: t -> let subset = powerset t in
		subset @ (append subset h);;

