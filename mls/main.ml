#use "tools/tools.ml";;

type user_info = { login : string; password : string };;
type data_file = user_info list;;

let rec is_duplicate_login (login: string) (lst: (string * string) list) : bool =
  if lst = [] then false
  else
    let (curr_login, _) = List.hd lst in
    if login = curr_login then true
    else is_duplicate_login login (List.tl lst)
;;

let rec remove_duplicates_by_login (pairs: (string * string) list) : (string * string) list =
  if pairs = [] then []
  else
    let (curr_login, curr_password) = List.hd pairs in
    let rest = List.tl pairs in
    if is_duplicate_login curr_login rest then remove_duplicates_by_login rest
    else (curr_login, curr_password) :: remove_duplicates_by_login rest
;;


(*let lines: (string * string) list = remove_duplicates (read_data_from_file "tools/test.txt");;*)

(* Main function to call *)
let rec parse_lines_to_users (lines: (string * string) list) : data_file =
  if lines = [] then []
  else
    let (login, password) = List.hd lines in
    { login; password } :: parse_lines_to_users (List.tl lines)
;;

parse_lines_to_users(lines);;

let read_and_parse_file filename =
  let lines = read_data_from_file filename in
  parse_lines_to_users lines
;;

read_and_parse_file "tools/test.txt"
