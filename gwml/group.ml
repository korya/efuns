(***********************************************************************)
(*                                                                     *)
(*                             GwML                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* user groups *)

open Gwml
open Stdconfig
  
type group = string
  
let client_groups = Wobenv.new_var ()
let known_groups = ref ([] : (string * group) list)
  
let find_group name =
  try
    List.assoc name !known_groups
  with Not_found ->
      let name = String.copy name in
      known_groups := (name, name) :: !known_groups;
      name

let find_wob w c =
  let (c,w) = Wintbl.find w.w_screen.s_clients c.c_window in w

let find_groups w =
  try Wob.getenv w.w_top client_groups
  with Not_found -> []

let rec print_groups_aux list = 
  match list with
    [] -> ()
  | (name,_) :: tail ->
      Printf.printf "G: %s" name; print_newline ();
      print_groups_aux tail

let print_groups () =
  Printf.printf "Groups:"; print_newline ();
  print_groups_aux !known_groups
      
let add_to_group name w = 
  let group = find_group name in
  let groups = find_groups w in
  if not (List.memq group groups) then
    Wob.setenv w.w_top client_groups (group :: groups)
    
let rec removeq_aux v list left =
  match list with
    [] -> left
  | v' :: tail -> 
        removeq_aux v tail (if v == v' then left else v' :: left)
    
let removeq v list = removeq_aux v list []
        
let remove_from_group name w =
  let group = find_group name in
  let groups = find_groups w in
  if List.memq group groups then
    Wob.setenv w.w_top client_groups (removeq group groups)
  else 
    (Printf.printf "Not in that group: %s" group; print_newline ())
    
let popup_new_group sw =
  assert (sw.w_parent = sw);
  find_group "default"

let group_menu action sw =
  (("New group ...", [], Function (fun sw ->
          let group = popup_new_group sw in
          action group sw)) ::
    (List.map (fun (name,group) ->
          name, [], Function (action group)) !known_groups))
    
let select_group popup_function action =
  popup_function 
    (("New group ...", [], Function (fun sw ->
        let group = popup_new_group sw in
        action group sw)) ::
    (List.map (fun (name,group) ->
          name, [], Function (action group)) !known_groups))
  
let iconify_group sw group =
  let group = find_group group in
  List.iter (fun (c,w) ->
      let groups = find_groups w in
      if List.memq group groups then
        Wob.send w.w_top (WobIconifyRequest true)
  ) (list_clients sw)
  
let main_group w =
  let groups = find_groups w in
  match groups with
    [] -> raise Not_found
  | group :: _ -> group
      
let clear_group group sw =
  let group = find_group group in
  List.iter (fun (c,w) ->
      let groups = find_groups w in
      if List.memq group groups then 
        Wob.setenv w.w_top client_groups (removeq group groups)
  ) (list_clients sw)
  