let gen zip uzip curry ucurry tmap tfor respectively mux demux n =
	let open List in
	let mkns lo n = let rec f x xs = if x>=lo then x::f (x-1) xs else xs in rev (f n []) in
	let ns = mkns 2 n in
	let lcat x = String.concat ", " x in
	let lf xs f = lcat (map f xs) in
	let ldress l r xs = lf xs (fun x->l^x^r) in
	let (^^) a b = a^"\n\t"^b in
	let g n = let xs = map string_of_int (mkns 1 n) in
		let m = string_of_int n in 
		let p x = "("^x^")" in
		let _x = ldress "_"  "" xs in
		let _xs = ldress "_" "s" xs in
		let _xxs = lf xs (fun x->"_"^x^"::_"^x^"s") in
		let _x_ = String.concat " " (map (fun x->"_"^x) xs) in
		let f_x = (ldress "f _" "" xs) in
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
		^"\nlet "^zip^m^"' "^_x_^" = "^zip^m^" ("^_x^")"
		^"\nlet "^curry^m^" f "^p _x^" = f "^_x_
		^"\nlet "^ucurry^m^" f "^_x_^" = f "^p _x
		^"\nlet "^tmap^m^" f "^p _x^" = "^p f_x 
		^"\nlet "^tmap^m^"' f "^_x_^" = "^p f_x
		^"\nlet "^tfor^m^" "^p _x^" f = "^p f_x
		^"\nlet "^tfor^m^"' "^_x_^" f = "^p f_x
	in String.concat "\n" (map g ns)


