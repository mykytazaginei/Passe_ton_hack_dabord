#use "tools/tools.ml";;

type user_info = { 
  login : string; 
  password : string 
}

type data_file = user_info list

let lines: (string * string) list = read_data_from_file "tools/test.txt";;