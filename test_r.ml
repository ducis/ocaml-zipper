let zipp2 xs = let rec f acc xx = match xx with
	(_1::_1s, _2::_2s) -> f ((_1, _2)::acc) (_1s, _2s)
	|_ -> acc
	in List.rev (f [] xs)
let uzipp2 xs = let rec f (_1s, _2s) xx = match xx with
	(_1, _2)::more -> f (_1::_1s, _2::_2s) more
	|_ -> (_1s, _2s)
	in let (_1s, _2s) = f ([], []) xs
	in (List.rev _1s, List.rev _2s)
let zipp2' _1 _2 = zipp2 (_1, _2)
let zipp3 xs = let rec f acc xx = match xx with
	(_1::_1s, _2::_2s, _3::_3s) -> f ((_1, _2, _3)::acc) (_1s, _2s, _3s)
	|_ -> acc
	in List.rev (f [] xs)
let uzipp3 xs = let rec f (_1s, _2s, _3s) xx = match xx with
	(_1, _2, _3)::more -> f (_1::_1s, _2::_2s, _3::_3s) more
	|_ -> (_1s, _2s, _3s)
	in let (_1s, _2s, _3s) = f ([], [], []) xs
	in (List.rev _1s, List.rev _2s, List.rev _3s)
let zipp3' _1 _2 _3 = zipp3 (_1, _2, _3)
let zipp4 xs = let rec f acc xx = match xx with
	(_1::_1s, _2::_2s, _3::_3s, _4::_4s) -> f ((_1, _2, _3, _4)::acc) (_1s, _2s, _3s, _4s)
	|_ -> acc
	in List.rev (f [] xs)
let uzipp4 xs = let rec f (_1s, _2s, _3s, _4s) xx = match xx with
	(_1, _2, _3, _4)::more -> f (_1::_1s, _2::_2s, _3::_3s, _4::_4s) more
	|_ -> (_1s, _2s, _3s, _4s)
	in let (_1s, _2s, _3s, _4s) = f ([], [], [], []) xs
	in (List.rev _1s, List.rev _2s, List.rev _3s, List.rev _4s)
let zipp4' _1 _2 _3 _4 = zipp4 (_1, _2, _3, _4)
let zipp5 xs = let rec f acc xx = match xx with
	(_1::_1s, _2::_2s, _3::_3s, _4::_4s, _5::_5s) -> f ((_1, _2, _3, _4, _5)::acc) (_1s, _2s, _3s, _4s, _5s)
	|_ -> acc
	in List.rev (f [] xs)
let uzipp5 xs = let rec f (_1s, _2s, _3s, _4s, _5s) xx = match xx with
	(_1, _2, _3, _4, _5)::more -> f (_1::_1s, _2::_2s, _3::_3s, _4::_4s, _5::_5s) more
	|_ -> (_1s, _2s, _3s, _4s, _5s)
	in let (_1s, _2s, _3s, _4s, _5s) = f ([], [], [], [], []) xs
	in (List.rev _1s, List.rev _2s, List.rev _3s, List.rev _4s, List.rev _5s)
let zipp5' _1 _2 _3 _4 _5 = zipp5 (_1, _2, _3, _4, _5)