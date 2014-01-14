#use "test_r.ml";;
let t x = print_endline (string_of_bool x) ;;
t (zipp2' [1;2;3;4] [5;6;7;8] = [(1,5);(2,6);(3,7);(4,8)]) ;;
t (uzipp3 [(1,2,3);(4,5,6)] = ([1;4],[2;5],[3;6])) ;;
