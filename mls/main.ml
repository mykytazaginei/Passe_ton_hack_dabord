#use "tools.ml";;

let hash = hash_password "password123";;

type user_info = { login : string; password : string };;
type data_file = user_info list;;

let lines: string*string list = read_data_from_file "tools/depensetout01.txt";;

let split_data(lines: string list): data_file = 
  