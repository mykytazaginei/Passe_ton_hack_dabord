#use "topfind";;
#require "cryptokit";;
#require "base64";;

let read_data_from_file file =
  let f = open_in file in
  let rec aux acc =
    try
      let login = input_line f 
      and pwd = input_line f
       in
        aux ((login, pwd) :: acc)
    with End_of_file ->
      close_in f;
    List.rev acc
  in
  aux [];; 
  
let read_passwords_from_file file =
  let f = open_in file in
  let rec aux acc =
    try
      let pwd = input_line f
        in
        aux (pwd :: acc)
    with End_of_file ->
      close_in f;
    List.rev acc
  in
  aux [];;  
  
let hash_password pwd =
  Base64.encode_exn(Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) pwd)
;;
