#use "tools/tools.ml";;

(* Ex. 1 : Fusionner dans une mˆeme variable les informations contenues dans les fichiers correspondant `a
plusieurs fuites d’une mˆeme application en ´eliminant les doublons (mˆeme login et mˆeme mot de
passe) *)

(* Update type to include file source *)

type user_info = { login: string; password: string };;
type file_data = user_info list;;
type key_value_list = (string * string) list;;

let rec parse_lines_to_users (lines: key_value_list) : file_data =
  if lines = [] then []
  else
    let (login, password) = List.hd lines in
    { login; password } :: parse_lines_to_users(List.tl lines)
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
  let users = parse_lines_to_users lines in
  remove_duplicates users
;;

(* Update main analysis function *)

(* Ex. 2 : d'eterminer si un même login est présent dans plusieurs fuites de données (donc dans les fichiers
correspondant à plusieurs applications web) et dans ce cas déterminer si les mots de passe sont
identiques *)

let merge_data_from_several_files (files: string list) : file_data =
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
let get_unique_logins (data: file_data) : string list =
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
let get_passwords_for_login (login: string) (data: file_data) : string list =
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
let are_passwords_identical (passwords: string list) : bool =
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
let analyze_data_leaks (filenames: string list) : unit =
  let all_data = merge_data_from_several_files filenames in
  let unique_logins = get_unique_logins all_data in
  let i = ref 0 in

  while !i < List.length unique_logins do
    let login = List.nth unique_logins !i in
    let passwords = get_passwords_for_login login all_data in

    if List.length passwords > 1 then begin
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

(* analyze_data_leaks ["tools/test.txt"; "tools/test2.txt"];; *)

(* Ex. 3 : déterminer si un même mot de passe haché est présent dans plusieurs fuites de données
et savoir à quels logins ils sont associés ; *)

let analyze_shared_passwords (filenames: string list) : unit =
  Printf.printf "Starting analysis...\n";
  
  (* Get all data from files *)
  Printf.printf "Loading data from files...\n";
  let all_data = merge_data_from_several_files filenames in
  Printf.printf "Loaded %d total entries\n" (List.length all_data);
  
  (* Recursive function to find or add group *)
  let rec find_group (password: string) (login: string) (groups: (string * string list) list) : (string * string list) list =
    if groups = [] then
      [(password, [login])]
    else 
      let (pass, logins) = List.hd groups in
      if pass = password then
        (pass, login :: logins) :: List.tl groups
      else
        (pass, logins) :: find_group password login (List.tl groups)
  in
  
  (* Recursive function to group passwords *)
  let rec group_passwords (data: file_data) (groups: (string * string list) list) : (string * string list) list =
    if data = [] then 
      groups
    else
      let entry = List.hd data in
      group_passwords (List.tl data) (find_group entry.password entry.login groups)
  in
  
  Printf.printf "Creating password groups...\n";
  let password_groups = group_passwords all_data [] in
  Printf.printf "Finished creating groups. Found %d unique passwords.\n" (List.length password_groups);
  
  (* Recursive function to print logins *)
  let rec print_logins (logins: string list) : unit =
    if logins = [] then
      Printf.printf "-------------------------\n"
    else begin 
      Printf.printf "- %s\n" (List.hd logins);
      print_logins (List.tl logins)
    end
  in
  
  (* Recursive function to print results *)
  let rec print_results (groups: (string * string list) list) (shared_count: int) : unit =
    if groups = [] then
      if shared_count = 0 then
        Printf.printf "No shared passwords found.\n"
      else
        Printf.printf "Found %d shared passwords.\n" shared_count
    else 
      let (password, logins) = List.hd groups in
      if List.length logins > 1 then begin
        Printf.printf "\nPassword '%s' is shared by these logins:\n" password;
        print_logins logins;
        print_results (List.tl groups) (shared_count + 1)
      end else
        print_results (List.tl groups) shared_count
  in
  
  Printf.printf "\nAnalyzing shared passwords...\n";
  Printf.printf "-------------------------\n";
  print_results password_groups 0
;;

analyze_shared_passwords ["tools/test.txt"; "tools/test2.txt"];;
