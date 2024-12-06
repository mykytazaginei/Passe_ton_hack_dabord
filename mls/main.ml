#use "tools/tools.ml";;

(** Types et structures de données pour l'authentification des utilisateurs et la gestion des fichiers.
  [user_info]: Type record contenant les détails d'authentification utilisateur
  [file_data]: Type liste pour stocker plusieurs enregistrements utilisateur
  [key_value_list]: Liste d'association pour les paires clé-valeur de type string
  Le record [user_info] contient:
  [login]: Identifiant/nom d'utilisateur
  [password]: Mot de passe de l'utilisateur
  [source]: Source/origine des données utilisateur
  [file_data] représente une collection d'enregistrements user_info stockés sous forme de liste.
  [key_value_list] est utilisé pour stocker des paires clé-valeur de type string dans une liste.
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO 
*)
type user_info = {
  login: string;
  password: string;
  source: string
}
type file_data = user_info list
type key_value_list = (string * string) list

(* Ex. 1 : fusionner dans une même variable les informations contenues dans les fichiers correspondant à
plusieurs fuites d'une même application en éliminant les doublons (même login et même mot de passe) *)

(** 
  Vérifie si un élément est présent dans une liste de chaînes de caractères.
  @param item La chaîne à rechercher dans la liste
  @param list La liste de chaînes dans laquelle effectuer la recherche 
  @return Renvoie true si l'élément est trouvé dans la liste, false sinon
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO 
*)
let rec is_in_list(item, list: string * string list): bool =
  if list = [] then false
  else if item = List.hd list then true
  else is_in_list (item, List.tl list)
;;

(** 
  Convertit récursivement une liste de paires clé-valeur [(login, password)] et une chaîne source en une liste d'enregistrements utilisateur.
  @param lines Liste de tuples contenant les paires login et mot de passe
  @param source Chaîne indiquant la source/origine des données utilisateur
  @return Liste d'enregistrements file_data, où chaque enregistrement contient login, mot de passe et source
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO 
*)
let rec parse_lines_to_users(lines, source: key_value_list * string): file_data =
  if lines = [] then []
  else
    let (login, password) = List.hd lines in
    { login; password; source; } :: parse_lines_to_users (List.tl lines, source)
;;

(** 
  Trie une liste de données de fichiers en utilisant l'algorithme du tri rapide.
  @param data La liste des données de fichiers à trier.
  @return La liste triée des données de fichiers.
  @author ZAGINEI Mykyta, HONCHARENKO Dmytro
*)
let quick_sort(data: file_data): file_data =
  let rec aux(lst: file_data): file_data =
    if lst = [] then []
    else
      let pivot = List.hd lst in
      let rest = List.tl lst in

      let rec partition(lst, less, greater: file_data * file_data * file_data): file_data * file_data =
        if lst = [] then (less, greater)
        else
          let current = List.hd lst in
          if current.login < pivot.login then
            partition (List.tl lst, current :: less, greater)
          else if current.login > pivot.login then
            partition (List.tl lst, less, current :: greater)
          else
            partition (List.tl lst, less, greater)
      in

      let (less, greater) = partition (rest, [], []) in
      aux less @ [pivot] @ aux greater
  in
  aux data
;;

(** 
  Supprime les doublons dans une liste de données de fichiers.
  @param data La liste des données de fichiers potentiellement avec des doublons
  @return La liste des données de fichiers sans doublons basés sur le login utilisateur
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO 
*)
let remove_duplicates(data: file_data): file_data =
  let sorted_data = quick_sort data in

  let rec aux(data, acc: file_data * file_data): file_data =
    if data = [] then acc
    else
      let current = List.hd data in
      let rest = List.tl data in
      if rest = [] || current.login <> (List.hd rest).login then
        aux (rest, current :: acc)
      else
        aux (rest, acc)
  in
  aux (sorted_data, [])
;;

(** 
  Lit un fichier et le convertit en file_data.
  @param filename Le nom du fichier à lire
  @return Les données du fichier sous forme de file_data avec login, mot de passe et source remplis pour chaque utilisateur
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO 
*)
let read_and_parse_file(filename: string): file_data =
  let lines = read_data_from_file filename in
  parse_lines_to_users (lines, filename)
;;

(** 
  Fusionne les données de plusieurs fichiers en une seule liste.
  @param files Liste des noms de fichiers à fusionner
  @return Les données fusionnées de tous les fichiers sous forme de file_data sans suppression des doublons pour l'instant
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO 
*)
let merge_data_from_several_files(files: string list): file_data =
  let rec aux(files, acc: string list * file_data): file_data =
    if files = [] then acc
    else
      let file = List.hd files in
      let file_data = read_and_parse_file file in
      aux (List.tl files, file_data @ acc)
  in
  aux (files, [])
;;

(** 
  Charge et traite les fichiers pour fusionner les données et supprimer les doublons.
  @param filenames Liste des noms de fichiers à traiter
  @return Les données fusionnées sans doublons sous forme de file_data triée par login utilisateur
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO 
*)
let load_and_process_files(filenames: string list): file_data =
  let data = merge_data_from_several_files filenames in
  remove_duplicates data
;;

load_and_process_files(["files/depensetout01.txt"; "files/depensetout02.txt"]);;

(* Ex. 2 : déterminer si un même login est présent dans plusieurs fuites de données (donc dans les fichiers
correspondant à plusieurs applications web) et dans ce cas déterminer si les mots de passe sont identiques *)

(** 
  Récupère la liste des logins uniques à partir des données.
  @param data Les données de fichiers
  @return La liste des logins uniques présents dans les données fournies sans doublons
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO 
*)
let get_unique_logins(data: file_data): string list =
  let rec aux(data, acc: file_data * string list): string list =
    if data = [] then acc
    else
      let current_login = (List.hd data).login in
      if not (is_in_list (current_login, acc)) then
        aux (List.tl data, current_login :: acc)
      else
        aux (List.tl data, acc)
  in
  aux (data, [])
;;

(** 
  Récupère toutes les entrées pour un login donné.
  @param login Le login à rechercher dans les données
  @param data Les données de fichiers
  @return La liste des enregistrements user_info correspondant au login donné dans les données fournies
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO
*)
let get_entries_for_login(login, data: string * file_data): user_info list =
  let rec aux(data, acc: file_data * user_info list): user_info list =
    if data = [] then acc
    else
      let entry = List.hd data in
      if entry.login = login then
        aux (List.tl data, entry :: acc)
      else
        aux (List.tl data, acc)
  in
  aux (data, [])
;;

(** 
  Vérifie si tous les mots de passe dans une liste sont identiques.
  @param passwords La liste des mots de passe à comparer
  @return Renvoie true si tous les mots de passe sont identiques, false sinon. Si la liste est vide ou contient un seul élément, renvoie true par défaut car aucun conflit n'est possible dans ce cas
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO
*)
let are_passwords_identical(passwords: string list): bool =
  let rec aux(passwords: string list): bool =
    if List.length passwords <= 1 then true
    else
      let pwd1 = List.hd passwords in
      let pwd2 = List.hd (List.tl passwords) in
      if pwd1 <> pwd2 then false
      else aux (List.tl passwords)
  in
  aux passwords
;;

(** 
  Traite les entrées pour un login et retourne la liste des mots de passe associés.
  @param entries La liste des enregistrements user_info pour un login spécifique
  @return La liste des mots de passe associés au login donné extraits des différentes sources/fuites de données. Affiche également les sources et mots de passe à l'écran pour l'utilisateur
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO
*)
let process_entries(entries: user_info list): string list =
  let rec aux(entries, acc: user_info list * string list): string list =
    if entries = [] then acc
    else
      let entry = List.hd entries in
      Printf.printf "Source: %s\n" entry.source;
      Printf.printf "Password: %s\n" entry.password;
      aux (List.tl entries, entry.password :: acc)
  in
  aux (entries, [])
;;

(** 
  Affiche les résultats de l'analyse pour un login spécifique.
  @param login Le login analysé
  @param passwords La liste des mots de passe associés au login
  @param identical Booléen indiquant si les mots de passe sont identiques ou non
  @return Ne retourne rien mais affiche les résultats à l'écran pour l'utilisateur. Indique si les mots de passe associés au login sont identiques ou différents entre les différentes fuites de données analysées pour ce login spécifique
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO
*)
let print_leak_results(login, passwords, identical: string * string list * bool): unit =
  Printf.printf "Login '%s' trouvé dans plusieurs fuites:\n" login;
  Printf.printf "Les mots de passe sont %s\n"
    (if identical then "identiques" else "différents");
  Printf.printf "-------------------------\n"
;;

(** 
  Analyse récursivement les logins pour détecter ceux présents dans plusieurs fuites et vérifier si les mots de passe sont identiques.
  @param logins La liste des logins uniques à analyser
  @param all_data Toutes les données fusionnées des différentes sources/fuites de données
  @return Ne retourne rien mais effectue l'affichage des résultats de l'analyse pour chaque login concerné. Permet de détecter les logins compromis sur plusieurs fuites et d'évaluer le niveau de risque (même mot de passe ou non entre les fuites)
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO
*)
let rec analyze_data_leaks_rec(logins, all_data: string list * file_data): unit =
  if logins = [] then ()
  else
    let login = List.hd logins in
    let entries = get_entries_for_login (login, all_data) in
    if List.length entries > 1 then begin
      Printf.printf "Login '%s' trouvé dans plusieurs fuites:\n" login;
      let passwords = process_entries entries in
      let identical = are_passwords_identical passwords in
      print_leak_results (login, passwords, identical)
    end;
    analyze_data_leaks_rec (List.tl logins, all_data)
;;

(** 
  Lance l'analyse des logins sur les fichiers fournis.
  @param filenames Liste des noms de fichiers à analyser
  @return Ne retourne rien mais effectue l'analyse complète des logins sur les différentes fuites de données fournies. Permet d'identifier les logins concernés par plusieurs fuites et de prendre des mesures de sécurité appropriées si nécessaire (changement de mot de passe, sensibilisation des utilisateurs, etc.)
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO
*)
let analyze_logins(filenames: string list): unit =
  let all_data = merge_data_from_several_files filenames in
  let unique_logins = get_unique_logins all_data in
  analyze_data_leaks_rec (unique_logins, all_data)
;;

analyze_logins(["files/slogram01.txt"; "files/slogram02.txt"]);;

(* Ex. 3 : déterminer si un même mot de passe haché est présent dans plusieurs fuites de données et savoir à quels logins ils sont associés ; *)

(** 
  Récupère la liste des mots de passe uniques à partir des données.
  @param data Les données de fichiers
  @return La liste des mots de passe uniques présents dans les données fournies sans doublons, permet de détecter les mots de passe les plus communs ou les plus compromis entre les différentes fuites de données analysées, ce qui peut aider à renforcer les politiques de sécurité et à sensibiliser les utilisateurs concernés
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO
*)
let get_unique_passwords(data: file_data): string list =
  let rec aux(data, acc: file_data * string list): string list =
    if data = [] then acc
    else
      let current = List.hd data in
      if not (is_in_list (current.password, acc)) then
        aux (List.tl data, current.password :: acc)
      else
        aux (List.tl data, acc)
  in
  aux (data, [])
;;

(** 
  Récupère les données associées à un mot de passe donné.
  @param password Le mot de passe haché à rechercher
  @param data Les données de fichiers
  @return La liste des enregistrements user_info correspondant au mot de passe haché donné. Permet de savoir quels utilisateurs sont concernés par un même mot de passe haché sur les différentes fuites de données analysées, ce qui peut indiquer des failles de sécurité ou des pratiques à risque (utilisation du même mot de passe sur plusieurs services, etc.)
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO
*)
let get_data_by_password(password, data: string * file_data): file_data =
  let rec aux(data, acc: file_data * file_data): file_data =
    if data = [] then acc
    else
      let entry = List.hd data in
      if entry.password = password then
        aux (List.tl data, entry :: acc)
      else
        aux (List.tl data, acc)
  in
  aux (data, [])
;;

(** 
  Affiche les données pour un mot de passe donné.
  @param data Les enregistrements user_info à afficher
  @return Ne retourne rien mais affiche à l'écran les logins et sources associés au mot de passe haché donné. Permet de visualiser les conséquences potentielles d'une fuite de mot de passe et de prendre des mesures correctives (changement de mot de passe, renforcement de la sécurité, etc.)
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO
*)
let rec print_password_data(data: file_data): unit =
  if data = [] then ()
  else
    let entry = List.hd data in
    Printf.printf "Login: %s from %s\n" entry.login entry.source;
    print_password_data (List.tl data)
;;

(** 
  Analyse récursivement les mots de passe hachés pour détecter ceux présents dans plusieurs fuites.
  @param passwords La liste des mots de passe hachés uniques à analyser
  @param all_data Toutes les données fusionnées des différentes sources/fuites de données
  @return Ne retourne rien mais affiche les résultats de l'analyse pour chaque mot de passe concerné. Permet de détecter les mots de passe les plus compromis et d'évaluer le niveau de risque associé pour les utilisateurs concernés
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO
*)
let rec analyze_passwords_hashed_rec(passwords, all_data: string list * file_data): unit =
  if passwords = [] then ()
  else
    let password = List.hd passwords in
    let data = get_data_by_password (password, all_data) in
    if List.length data > 1 then
      (
        Printf.printf "Password hash '%s' est dans:\n" password;
        print_password_data data;
        Printf.printf "-------------------------\n"
      );
    analyze_passwords_hashed_rec (List.tl passwords, all_data)
;;

(** 
  Lance l'analyse des mots de passe hachés sur les fichiers fournis.
  @param files Liste des noms de fichiers à analyser
  @return Ne retourne rien mais effectue l'analyse complète des mots de passe hachés sur les différentes fuites de données fournies. Aide à identifier les mots de passe les plus compromis et à prendre des mesures de sécurité appropriées si nécessaire
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO
*)
let analyze_passwords_hashed(files: string list): unit =
  let all_data = merge_data_from_several_files files in
  let unique_passwords = get_unique_passwords all_data in
  analyze_passwords_hashed_rec (unique_passwords, all_data)
;;

analyze_passwords_hashed(["files/tetedamis01.txt"; "files/tetedamis02.txt"]);;

(* Ex. 4 : Etant donnée une liste de mots de passe en clair, extraire la liste des couples (application web, login) pour lequel le mot de passe haché associé au login correspond au haché d'un des mots de passe en clair. *)

(** 
  Hache une liste de mots de passe en clair.
  @param passwords La liste des mots de passe en clair à hacher
  @return La liste des mots de passe hachés correspondants. Utilise la fonction hash_password du module tools.ml pour effectuer le hachage
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO
*)
let hash_unecrypted_passwords(passwords: string list): string list =
  let rec aux(passwords, hashed_passwords: string list * string list): string list =
    if passwords = [] then hashed_passwords
    else
      aux (List.tl passwords, hash_password(List.hd passwords) :: hashed_passwords)
  in
  aux(passwords, [])
;;

(** 
  Analyse les mots de passe hachés à partir d'une liste de mots de passe en clair et affiche les couples (application web, login).
  @param unhashed_passwords_file Le nom du fichier contenant les mots de passe en clair
  @param all_data_files La liste des fichiers de données à analyser
  @return Ne retourne rien mais affiche les résultats de l'analyse sur les couples (application web, login) pour lesquels le mot de passe haché correspond au haché d'un mot de passe en clair fourni. Permet d'identifier les utilisateurs dont le mot de passe est compromis et de prendre des mesures de sécurité appropriées si nécessaire
  @author Mykyta ZAGINEI, Dmytro HONCHARENKO
*)
let analyze_hash_with_unhashed_passwords(unhashed_passwords_file, all_data_files: string * string list) =
  let unhashed_passwords = read_passwords_from_file unhashed_passwords_file in
  let all_data = merge_data_from_several_files all_data_files in

  let rec aux(unhashed_passwords, hashed_passwords: string list * string list): unit =
    if unhashed_passwords = [] then ()
    else
      let unhashed_password = List.hd unhashed_passwords in
      let hashed_password = List.hd hashed_passwords in
      let data = get_data_by_password (hashed_password, all_data) in
      if data <> [] then
        (
          Printf.printf "Password '%s' (hash: '%s') est dans:\n" unhashed_password hashed_password;
          print_password_data data;
          Printf.printf "-------------------------\n"
        );
      aux (List.tl unhashed_passwords, List.tl hashed_passwords)
  in
  aux (unhashed_passwords, hash_unecrypted_passwords unhashed_passwords)
;;

analyze_hash_with_unhashed_passwords("files/french_passwords_top20000.txt", ["files/depensetout02.txt"; "files/slogram01.txt"]);; 
