#use "tools/tools.ml";;

(* Update type to include file source *)
type user_info = { login : string; password : string; source : string };;
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

(* Update parse function to include source *)
let rec parse_lines_to_users (lines: (string * string) list) (source: string) : data_file =
  if lines = [] then []
  else
    let (login, password) = List.hd lines in
    { login; password; source } :: parse_lines_to_users (List.tl lines) source
;;

let read_and_parse_file filename =
  let lines = read_data_from_file filename in
  parse_lines_to_users lines filename
;;

(* Function to merge data from multiple files *)
let merge_data_files (files: string list) : data_file =
  let result = ref [] in
  let current_files = ref files in
  while !current_files <> [] do
    let file = List.hd !current_files in
    let file_data = read_and_parse_file file in
    let current_data = ref file_data in
    while !current_data <> [] do
      result := List.hd !current_data :: !result;
      current_data := List.tl !current_data
    done;
    current_files := List.tl !current_files
  done;
  !result
;;

(* Update group_by_login to track sources *)
let group_by_login (data: data_file) : (string * (string * string) list) list =
  let result = ref [] in
  for i = 0 to List.length data - 1 do
    let entry = List.nth data i in
    let found = ref false in
    let groups_len = List.length !result in
    for j = 0 to groups_len - 1 do
      let (login, pass_sources) = List.nth !result j in
      if login = entry.login then begin
        result := List.take j !result @
                 [(login, (entry.password, entry.source) :: pass_sources)] @
                 List.drop (j + 1) !result;
        found := true
      end
    done;
    if not !found then
      result := !result @ [(entry.login, [(entry.password, entry.source)])]
  done;
  !result
;;

(* Update analyze_duplicates for source tracking *)
let analyze_duplicates (grouped_data: (string * (string * string) list) list) =
  List.filter (fun (_, pass_sources) -> List.length pass_sources > 1) grouped_data |>
  List.map (fun (login, pass_sources) ->
    let unique_passwords = List.sort_uniq compare (List.map fst pass_sources) in
    (login, pass_sources, List.length unique_passwords = 1)
  )
;;

(* Update main analysis function with detailed output *)
let analyze_data_leaks (filenames: string list) =
  let all_data = merge_data_files filenames in
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
