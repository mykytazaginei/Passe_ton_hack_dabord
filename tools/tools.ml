(*Mykyta ZAGINEI, Dmytro HONCHARENKO*)
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

(** 
  Lit un fichier contenant des mots de passe et les retourne sous forme de liste.
  @param file Le nom du fichier à lire
  @return Une liste de mots de passe lus depuis le fichier. Chaque mot de passe correspond à une ligne du fichier. Le fichier est lu jusqu'à la fin, et les mots de passe sont retournés dans l'ordre original.
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO 
*)
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
