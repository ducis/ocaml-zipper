let zipp2 = let rec f acc xx = match xx with
	(_1::_1s, _2::_2s) -> f ((_1, _2)::acc) (_1s, _2s)
	|_ -> acc
	in List.rev (f [] xs)
let uzipp2 = let rec f (_1s', _2s') xx = match xx with
	(_1, _2)::more -> f (_1::_1s', _2::_2s') more
	|_ -> (_1s', _2s')
	in let (_1s, _2s) = f ([], []) xs
	in (List.rev _1, List.rev _2)
let zipp3 = let rec f acc xx = match xx with
	(_1::_1s, _2::_2s, _3::_3s) -> f ((_1, _2, _3)::acc) (_1s, _2s, _3s)
	|_ -> acc
	in List.rev (f [] xs)
let uzipp3 = let rec f (_1s', _2s', _3s') xx = match xx with
	(_1, _2, _3)::more -> f (_1::_1s', _2::_2s', _3::_3s') more
	|_ -> (_1s', _2s', _3s')
	in let (_1s, _2s, _3s) = f ([], [], []) xs
	in (List.rev _1, List.rev _2, List.rev _3)
let zipp4 = let rec f acc xx = match xx with
	(_1::_1s, _2::_2s, _3::_3s, _4::_4s) -> f ((_1, _2, _3, _4)::acc) (_1s, _2s, _3s, _4s)
	|_ -> acc
	in List.rev (f [] xs)
let uzipp4 = let rec f (_1s', _2s', _3s', _4s') xx = match xx with
	(_1, _2, _3, _4)::more -> f (_1::_1s', _2::_2s', _3::_3s', _4::_4s') more
	|_ -> (_1s', _2s', _3s', _4s')
	in let (_1s, _2s, _3s, _4s) = f ([], [], [], []) xs
	in (List.rev _1, List.rev _2, List.rev _3, List.rev _4)
let zipp5 = let rec f acc xx = match xx with
	(_1::_1s, _2::_2s, _3::_3s, _4::_4s, _5::_5s) -> f ((_1, _2, _3, _4, _5)::acc) (_1s, _2s, _3s, _4s, _5s)
	|_ -> acc
	in List.rev (f [] xs)
let uzipp5 = let rec f (_1s', _2s', _3s', _4s', _5s') xx = match xx with
	(_1, _2, _3, _4, _5)::more -> f (_1::_1s', _2::_2s', _3::_3s', _4::_4s', _5::_5s') more
	|_ -> (_1s', _2s', _3s', _4s', _5s')
	in let (_1s, _2s, _3s, _4s, _5s) = f ([], [], [], [], []) xs
	in (List.rev _1, List.rev _2, List.rev _3, List.rev _4, List.rev _5)
let zipp6 = let rec f acc xx = match xx with
	(_1::_1s, _2::_2s, _3::_3s, _4::_4s, _5::_5s, _6::_6s) -> f ((_1, _2, _3, _4, _5, _6)::acc) (_1s, _2s, _3s, _4s, _5s, _6s)
	|_ -> acc
	in List.rev (f [] xs)
let uzipp6 = let rec f (_1s', _2s', _3s', _4s', _5s', _6s') xx = match xx with
	(_1, _2, _3, _4, _5, _6)::more -> f (_1::_1s', _2::_2s', _3::_3s', _4::_4s', _5::_5s', _6::_6s') more
	|_ -> (_1s', _2s', _3s', _4s', _5s', _6s')
	in let (_1s, _2s, _3s, _4s, _5s, _6s) = f ([], [], [], [], [], []) xs
	in (List.rev _1, List.rev _2, List.rev _3, List.rev _4, List.rev _5, List.rev _6)
let zipp7 = let rec f acc xx = match xx with
	(_1::_1s, _2::_2s, _3::_3s, _4::_4s, _5::_5s, _6::_6s, _7::_7s) -> f ((_1, _2, _3, _4, _5, _6, _7)::acc) (_1s, _2s, _3s, _4s, _5s, _6s, _7s)
	|_ -> acc
	in List.rev (f [] xs)
let uzipp7 = let rec f (_1s', _2s', _3s', _4s', _5s', _6s', _7s') xx = match xx with
	(_1, _2, _3, _4, _5, _6, _7)::more -> f (_1::_1s', _2::_2s', _3::_3s', _4::_4s', _5::_5s', _6::_6s', _7::_7s') more
	|_ -> (_1s', _2s', _3s', _4s', _5s', _6s', _7s')
	in let (_1s, _2s, _3s, _4s, _5s, _6s, _7s) = f ([], [], [], [], [], [], []) xs
	in (List.rev _1, List.rev _2, List.rev _3, List.rev _4, List.rev _5, List.rev _6, List.rev _7)
let zipp8 = let rec f acc xx = match xx with
	(_1::_1s, _2::_2s, _3::_3s, _4::_4s, _5::_5s, _6::_6s, _7::_7s, _8::_8s) -> f ((_1, _2, _3, _4, _5, _6, _7, _8)::acc) (_1s, _2s, _3s, _4s, _5s, _6s, _7s, _8s)
	|_ -> acc
	in List.rev (f [] xs)
let uzipp8 = let rec f (_1s', _2s', _3s', _4s', _5s', _6s', _7s', _8s') xx = match xx with
	(_1, _2, _3, _4, _5, _6, _7, _8)::more -> f (_1::_1s', _2::_2s', _3::_3s', _4::_4s', _5::_5s', _6::_6s', _7::_7s', _8::_8s') more
	|_ -> (_1s', _2s', _3s', _4s', _5s', _6s', _7s', _8s')
	in let (_1s, _2s, _3s, _4s, _5s, _6s, _7s, _8s) = f ([], [], [], [], [], [], [], []) xs
	in (List.rev _1, List.rev _2, List.rev _3, List.rev _4, List.rev _5, List.rev _6, List.rev _7, List.rev _8)
let zipp9 = let rec f acc xx = match xx with
	(_1::_1s, _2::_2s, _3::_3s, _4::_4s, _5::_5s, _6::_6s, _7::_7s, _8::_8s, _9::_9s) -> f ((_1, _2, _3, _4, _5, _6, _7, _8, _9)::acc) (_1s, _2s, _3s, _4s, _5s, _6s, _7s, _8s, _9s)
	|_ -> acc
	in List.rev (f [] xs)
let uzipp9 = let rec f (_1s', _2s', _3s', _4s', _5s', _6s', _7s', _8s', _9s') xx = match xx with
	(_1, _2, _3, _4, _5, _6, _7, _8, _9)::more -> f (_1::_1s', _2::_2s', _3::_3s', _4::_4s', _5::_5s', _6::_6s', _7::_7s', _8::_8s', _9::_9s') more
	|_ -> (_1s', _2s', _3s', _4s', _5s', _6s', _7s', _8s', _9s')
	in let (_1s, _2s, _3s, _4s, _5s, _6s, _7s, _8s, _9s) = f ([], [], [], [], [], [], [], [], []) xs
	in (List.rev _1, List.rev _2, List.rev _3, List.rev _4, List.rev _5, List.rev _6, List.rev _7, List.rev _8, List.rev _9)
let zipp10 = let rec f acc xx = match xx with
	(_1::_1s, _2::_2s, _3::_3s, _4::_4s, _5::_5s, _6::_6s, _7::_7s, _8::_8s, _9::_9s, _10::_10s) -> f ((_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)::acc) (_1s, _2s, _3s, _4s, _5s, _6s, _7s, _8s, _9s, _10s)
	|_ -> acc
	in List.rev (f [] xs)
let uzipp10 = let rec f (_1s', _2s', _3s', _4s', _5s', _6s', _7s', _8s', _9s', _10s') xx = match xx with
	(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)::more -> f (_1::_1s', _2::_2s', _3::_3s', _4::_4s', _5::_5s', _6::_6s', _7::_7s', _8::_8s', _9::_9s', _10::_10s') more
	|_ -> (_1s', _2s', _3s', _4s', _5s', _6s', _7s', _8s', _9s', _10s')
	in let (_1s, _2s, _3s, _4s, _5s, _6s, _7s, _8s, _9s, _10s) = f ([], [], [], [], [], [], [], [], [], []) xs
	in (List.rev _1, List.rev _2, List.rev _3, List.rev _4, List.rev _5, List.rev _6, List.rev _7, List.rev _8, List.rev _9, List.rev _10)