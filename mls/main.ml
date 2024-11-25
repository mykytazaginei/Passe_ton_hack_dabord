#use "tools/tools.ml";;

type login = string
type password = string
type filename = string
type source = string
type user_info = {
  login: login;
  password: password;
  source: source
}
type file_data = user_info list
type key_value_list = (login * password) list

let rec parse_lines_to_users (lines: key_value_list) (source: filename) : file_data =
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

(* Update main analysis function *)

(* Ex. 2 : d'eterminer si un même login est présent dans plusieurs fuites de données (donc dans les fichiers
correspondant à plusieurs applications web) et dans ce cas déterminer si les mots de passe sont
identiques *)

let merge_data_from_several_files (files: filename list) : file_data =
  let result = ref [] in
  let i = ref 0 in

  while !i < List.length files do
    let file = List.nth files !i in
    let file_data = read_and_parse_file file in

    let j = ref 0 in
    while !j < List.length file_data do
      let entry = List.nth file_data !j in
      result := entry :: !result;
      j := !j + 1
    done;
    i := !i + 1
  done;
  !result
;;

(* Helper function to create a list of unique logins *)
let get_unique_logins (data: file_data) : login list =
  let result = ref [] in
  let i = ref 0 in

  while !i < List.length data do
    let current = List.nth data !i in
    let is_duplicate = ref false in
    let j = ref 0 in

    while !j < List.length !result do
      if current.login = List.nth !result !j then
        is_duplicate := true;
      j := !j + 1
    done;

    if not !is_duplicate then
      result := current.login :: !result;
    i := !i + 1
  done;
  !result
;;

(* Helper function to get all passwords for a login *)
let get_passwords_for_login (login: login) (data: file_data) : password list =
  let result = ref [] in
  let i = ref 0 in

  while !i < List.length data do
    let entry = List.nth data !i in
    if entry.login = login then
      result := entry.password :: !result;
    i := !i + 1
  done;
  !result
;;

(* Helper function to check if all passwords in a list are identical *)
let are_passwords_identical (passwords: password list) : bool =
  if List.length passwords <= 1 then true
  else
    let first_password = List.hd passwords in
    let is_identical = ref true in
    let i = ref 1 in

    while !i < List.length passwords do
      if List.nth passwords !i <> first_password then
        is_identical := false;
      i := !i + 1
    done;
    !is_identical
;;

(* Main function to analyze data leaks *)
let analyze_data_leaks (filenames: filename list) : unit =
  let all_data = merge_data_from_several_files filenames in
  let unique_logins = get_unique_logins all_data in
  let i = ref 0 in

  while !i < List.length unique_logins do
    let login = List.nth unique_logins !i in
    let passwords = get_passwords_for_login login all_data in
    (* Printf.printf "%d passwords length\n" (List.length passwords); *)
    if List.length passwords > 0 then begin
      Printf.printf "\nLogin '%s' found in multiple leaks:\n" login;
      let j = ref 0 in
      while !j < List.length passwords do
        Printf.printf "Password: %s\n" (List.nth passwords !j);
        j := !j + 1
      done;

      let identical = are_passwords_identical passwords in
      Printf.printf "Passwords are %s\n"
        (if identical then "identical" else "different");
      Printf.printf "-------------------------\n";
    end;

    i := !i + 1
  done
;;

analyze_data_leaks(["tools/test.txt"; "tools/test2.txt"]);; 

(* Ex. 3 : déterminer si un même mot de passe haché est présent dans plusieurs fuites de données
et savoir à quels logins ils sont associés ; *)
(*get unique passwords*)
let get_unique_passwords (data: file_data) : password list =
  let result: password list ref = ref [] in
  let i: int ref = ref 0 in

  while !i < List.length data do
    let current: user_info = List.nth data !i in
    let is_duplicate: bool ref = ref false in
    let j: int ref = ref 0 in

    while !j < List.length !result do
      if current.password = List.nth !result !j then
        is_duplicate := true;
      j := !j + 1
    done;

    if not !is_duplicate then
      result := current.password :: !result;
    i := !i + 1
  done;
  !result
;;

let get_data_by_password (password: password) (data: file_data) : file_data =
  let result: file_data ref = ref [] in
  let i: int ref = ref 0 in
  while !i < List.length data do
    let entry: user_info = List.nth data !i in
    if entry.password = password then
      result := entry :: !result;
    i := !i + 1
  done;
  !result
;;

(* let get_data_by_login (login: login) (data: file_data) : file_data =
  let result: file_data ref = ref [] in
  let i: int ref = ref 0 in
  while !i < List.length data do
    let entry: user_info = List.nth data !i in
    if entry.login = login then
      result := entry :: !result;
    i := !i + 1
  done;
  !result
;; *)

let analyze_passwords_hashed(files : filename list): unit =
  let all_data: file_data = merge_data_from_several_files(files) in
  let unique_passwords: password list = get_unique_passwords(all_data) in
  let i: int ref = ref 0 in

  while !i < List.length unique_passwords do
    let password: password = List.nth unique_passwords !i in
    let data: file_data = get_data_by_password password all_data in
    if List.length data > 1 then
      (
      Printf.printf "Password hash '%s' est dans:\n" password;
      let j: int ref = ref 0 in
      while !j < List.length data do
        Printf.printf "Login: %s from %s\n" (List.nth data !j).login (List.nth data !j).source;
        j := !j + 1
      done;
      );
    i := !i + 1
  done
;;

analyze_passwords_hashed(["tools/test.txt"; "tools/test2.txt"]);;

(* Ex. 4 : Etant donnée une liste de mots de passe en clair, extraire la liste des couples (application web, ´
login) pour lequel le mot de passe hach´e associ´e au login correspond au hach´e d’un des mots de
passe en clair. *)

let hash_unecrypted_passwords(passwords: password list): password list =
  let rec aux(passwords, hashed_passwords: password list * password list): password list = 
    if passwords = [] then hashed_passwords
    else 
      aux (List.tl passwords, hash_password(List.hd passwords) :: hashed_passwords)
  in
  aux(passwords, [])
;;

let rec parse_lines_to_passwords (lines: password list) : password list =
  if lines = [] then []
  else
    let password: password = List.hd lines in
    password  :: parse_lines_to_passwords(List.tl lines)
;;

let read_and_parse_file_passwords (filename : string) : password list =
  let lines : password list = read_passwords_from_file filename in
  parse_lines_to_passwords lines
;;