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

(* Function to merge data from multiple files *)
let merge_data_from_several_files (files: string list) : file_data =
  let result = ref [] in
  let i = ref 0 in
  
  while !i < List.length files do
    let file = List.nth files !i in
    let file_data = read_and_parse_file file in
    
    (* Append current file's data to result *)
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

let group_by_login (data: file_data) : (string * key_value_list) list =
  let result = ref [] in
  let i = ref 0 in
  
  while !i < List.length data do
    let entry = List.nth data !i in
    let found = ref false in
    let j = ref 0 in
    
    (* Check if login already exists in result *)
    while !j < List.length !result do
      let (login, passwords) = List.nth !result !j in
      if login = entry.login then begin
        (* Update existing entry *)
        let new_passwords = (entry.login, entry.password) :: passwords in
        result := !result @ [(login, new_passwords)];
        found := true;
      end;
      j := !j + 1
    done;
    
    (* If login not found, add new entry *)
    if not !found then
      result := !result @ [(entry.login, [(entry.login, entry.password)])];
    
    i := !i + 1
  done;
  
  !result
;;
(*
(* Update main analysis function with detailed output *)
let analyze_data_leaks (filenames: string list) =
  let all_data = merge_data_from_several_files filenames in
  let grouped = group_by_login all_data in
  let duplicates = analyze_duplicates grouped in
  List.iter (fun (login, pass_sources, same_password) ->
    Printf.printf "\nLogin '%s' found in %d leaks:\n" login (List.length pass_sources);
    List.iter (fun (pass, source) ->
      Printf.printf "- File: %s, Password: %s\n" source pass
    ) pass_sources;
    Printf.printf "Passwords are %s\n" (if same_password then "identical" else "different");
    Printf.printf "-------------------------\n"
  ) duplicates
;;

(* Example usage *)
let () = analyze_data_leaks ["tools/tetedamis01.txt"; "tools/tetedamis02.txt"];;
*)

