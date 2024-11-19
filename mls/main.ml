#use "tools/tools.ml";;

type user_info = { login : string; password : string };;
type data_file = user_info list;;

let lines: (string*string) list = read_data_from_file "tools/depensetout01.txt";;
  
let rec parse_lines_to_users (lines: (string*string) list): data_file =
  if lines = [] then
    []
  else if List.length lines < 2 then
    failwith "ffffeffq"
  else
    let login = List.hd lines in
    let tail = List.tl lines in
    let password = List.hd tail in
    let remaining = List.tl tail in
    { login; password } :: parse_lines_to_users remaining
;;

parse_lines_to_users(lines);;
