#use "tools/tools.ml";;

type user_info = { login : string; password : string };;
type data_file = user_info list;;

let lines: (string * string) list = read_data_from_file "tools/test.txt";;
  
let rec parse_lines_to_users (lines: (string * string) list) : data_file =
  if lines = [] then []
  else
    let (login, password) = List.hd lines in
    { login; password } :: parse_lines_to_users (List.tl lines)
;;

parse_lines_to_users(lines);;
