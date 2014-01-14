let gen zip uzip n =
	let open List in
	let mkns lo n = let rec f x xs = if x>=lo then x::f (x-1) xs else xs in rev (f n []) in
	let ns = mkns 2 n in
	let lcat x = String.concat ", " x in
	let lf xs f = lcat (map f xs) in
	let ldress l r xs = lf xs (fun x->l^x^r) in
	let (^^) a b = a^"\n\t"^b in
	let g n = let xs = map string_of_int (mkns 1 n) in
		let m = string_of_int n in 
		let _x = ldress "_"  "" xs in
		let _xs = ldress "_" "s" xs in
		let _xxs = lf xs (fun x->"_"^x^"::_"^x^"s") in
		(*let _xs' = ldress "_" "s'" xs in*)
		(*let _xxs' = lf xs (fun x->"_"^x^"::_"^x^"s'") in*)
		"let "^zip^m^" xs = let rec f acc xx = match xx with"
		^^"("^_xxs^") -> f (("^_x^")::acc) ("^_xs^")"
		^^"|_ -> acc"
		^^"in List.rev (f [] xs)"
		^"\nlet "^uzip^m^" xs = let rec f ("^_xs^") xx = match xx with"
		^^"("^_x^")::more -> f ("^_xxs^") more"
		^^"|_ -> ("^_xs^")"
		^^"in let ("^_xs^") = f ("^lf xs (fun _->"[]")^") xs"
		^^"in ("^ldress "List.rev _" "s" xs^")"
		^"\nlet "^zip^m^"' "^String.concat " " (map (fun x->"_"^x) xs)^" = "^zip^m^" ("^_x^")"
	in String.concat "\n" (map g ns)
