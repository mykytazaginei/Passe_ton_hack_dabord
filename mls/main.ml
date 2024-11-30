#use "tools/tools.ml";;

type user_info = {
  login: string;
  password: string;
  source: string
}
type file_data = user_info list
type key_value_list = (string * string) list

(* Ex. 1 : fusionner dans une même variable les informations contenues dans les fichiers correspondant à
plusieurs fuites d'une même application en éliminant les doublons (même login et même mot de
passe) *)

let rec is_in_list (item: string) (list: string list) : bool =
  if list = [] then false
  else if item = List.hd list then true
  else is_in_list item (List.tl list)
;;

let rec parse_lines_to_users (lines: key_value_list) (source: string) : file_data =
  if lines = [] then []
  else
    let (login, password) = List.hd lines in
    { login; password; source; } :: parse_lines_to_users (List.tl lines) source
;;

let remove_duplicates (data: file_data) : file_data =
  let result = ref [] in
  let i = ref 0 in

  while !i < List.length data do
    let current = List.nth data !i in
    let is_duplicate = ref false in
    let j = ref 0 in

    (* Check if this login already exists in result *)
    while !j < List.length !result do
      let existing = List.nth !result !j in
      if existing.login = current.login then
        is_duplicate := true;
      j := !j + 1
    done;

    (* Add to result only if not a duplicate *)
    if not !is_duplicate then
      result := current :: !result;

    i := !i + 1
  done;
  !result
;;

let read_and_parse_file (filename : string) : file_data =
  let lines = read_data_from_file filename in
  let users = parse_lines_to_users lines filename in
  remove_duplicates users
;;

(* Ex. 2 : déterminer si un même login est présent dans plusieurs fuites de données (donc dans les fichiers
correspondant à plusieurs applications web) et dans ce cas déterminer si les mots de passe sont
identiques *)

let merge_data_from_several_files (files: string list) : file_data =
  let rec aux (files: string list) (acc: file_data) : file_data =
    if files = [] then acc
    else
      let file = List.hd files in
      let file_data = read_and_parse_file file in
      aux (List.tl files) (file_data @ acc)
  in
  aux files []
;;

(* Helper function to create a list of unique logins *)
let get_unique_logins (data: file_data) : string list =
  let rec aux (data: file_data) (acc: string list) : string list =
    if data = [] then acc
    else
      let current_login = (List.hd data).login in
      if not (is_in_list current_login acc) then
        aux (List.tl data) (current_login :: acc)
      else
        aux (List.tl data) acc
  in
  aux data []
;;

(* Helper function to get all entries for a given login *)
let get_entries_for_login (login: string) (data: file_data) : user_info list =
  let rec aux (data: file_data) (acc: user_info list) : user_info list =
    if data = [] then acc
    else
      let entry = List.hd data in
      if entry.login = login then
        aux (List.tl data) (entry :: acc)
      else
        aux (List.tl data) acc
  in
  aux data []
;;

(* Helper function to check if all passwords in a list are identical *)
let are_passwords_identical (passwords: string list) : bool =
  let rec aux (passwords: string list) : bool =
    if List.length passwords <= 1 then true
    else
      let pwd1 = List.hd passwords in
      let pwd2 = List.hd (List.tl passwords) in
      if pwd1 <> pwd2 then false
      else aux (List.tl passwords)
  in
  aux passwords
;;

let process_entries (entries: user_info list) : string list =
  let rec aux (entries: user_info list) (acc: string list) : string list =
    if entries = [] then acc
    else
      let entry = List.hd entries in
      Printf.printf "Source: %s\n" entry.source;
      Printf.printf "Password: %s\n" entry.password;
      aux (List.tl entries) (entry.password :: acc)
  in
  aux entries []
;;

let print_leak_results (login: string) (passwords: string list) (identical: bool) : unit =
  Printf.printf "Login '%s' trouvé dans plusieurs fuites:\n" login;
  Printf.printf "Les mots de passe sont %s\n"
    (if identical then "identiques" else "différents");
  Printf.printf "-------------------------\n"
;;

let rec analyze_data_leaks_rec (logins: string list) (all_data: file_data) : unit =
  if logins = [] then ()
  else
    let login = List.hd logins in
    let entries = get_entries_for_login login all_data in
    if List.length entries > 1 then begin
      Printf.printf "Login '%s' trouvé dans plusieurs fuites:\n" login;
      let passwords = process_entries entries in
      let identical = are_passwords_identical passwords in
      print_leak_results login passwords identical
    end;
    analyze_data_leaks_rec (List.tl logins) all_data
;;

let analyze_data_leaks (filenames: string list) : unit =
  let all_data = merge_data_from_several_files filenames in
  let unique_logins = get_unique_logins all_data in
  analyze_data_leaks_rec unique_logins all_data
;;

analyze_data_leaks(["tools/test.txt"; "tools/test2.txt"]);;

(* Ex. 3 : déterminer si un même mot de passe haché est présent dans plusieurs fuites de données
et savoir à quels logins ils sont associés ; *)

let get_unique_passwords (data: file_data) : string list =
  let rec aux (data: file_data) (acc: string list) : string list =
    if data = [] then acc
    else
      let current = List.hd data in
      if not (is_in_list current.password acc) then
        aux (List.tl data) (current.password :: acc)
      else
        aux (List.tl data) acc
  in
  aux data []
;;

let get_data_by_password (password: string) (data: file_data) : file_data =
  let rec aux (data: file_data) (acc: file_data) : file_data =
    if data = [] then acc
    else
      let entry = List.hd data in
      if entry.password = password then
        aux (List.tl data) (entry :: acc)
      else
        aux (List.tl data) acc
  in
  aux data []
;;

let rec print_password_data (data: file_data) : unit =
  if data = [] then ()
  else
    let entry = List.hd data in
    Printf.printf "Login: %s from %s\n" entry.login entry.source;
    print_password_data (List.tl data)
;;

let rec analyze_passwords_hashed_rec (passwords: string list) (all_data: file_data) : unit =
  if passwords = [] then ()
  else
    let password = List.hd passwords in
    let data = get_data_by_password password all_data in
    if List.length data > 1 then
      (
        Printf.printf "Password hash '%s' est dans:\n" password;
        print_password_data data
      );
    analyze_passwords_hashed_rec (List.tl passwords) all_data
;;

let analyze_passwords_hashed (files: string list) : unit =
  let all_data = merge_data_from_several_files files in
  let unique_passwords = get_unique_passwords all_data in
  analyze_passwords_hashed_rec unique_passwords all_data
;;

analyze_passwords_hashed(["tools/test.txt"; "tools/test2.txt"]);;

(* Ex. 4 : Etant donnée une liste de mots de passe en clair, extraire la liste des couples (application web,
login) pour lequel le mot de passe haché associé au login correspond au haché d'un des mots de
passe en clair. *)

let hash_unecrypted_passwords(passwords: string list): string list =
  let rec aux(passwords, hashed_passwords: string list * string list): string list =
    if passwords = [] then hashed_passwords
    else
      aux (List.tl passwords, hash_password(List.hd passwords) :: hashed_passwords)
  in
  aux(passwords, [])
;;

let rec parse_lines_to_passwords (lines: string list) : string list =
  if lines = [] then []
  else
    let password: string = List.hd lines in
    password  :: parse_lines_to_passwords(List.tl lines)
;;

let read_and_parse_file_passwords (filename : string) : string list =
  let lines : string list = read_passwords_from_file filename in
  parse_lines_to_passwords lines
;;
