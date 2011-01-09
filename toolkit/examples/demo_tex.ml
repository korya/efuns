(***********************************************************************)
(*                                                                     *)
(*                             ____                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)



open Xtypes
open WX_types
open WX_tree

let path = ref [ ]

let display = new WX_display.t ""
let root = new WX_root.t display 0
let top = new WX_wmtop.t root [MinWidth 10; MinHeight 10]
let tree = new WX_tree.t top#container []

let regexp = Str.regexp "\(\\input\|\\section\|\\subsection\|\\subsubsection\){\([^}]+\)}"

let rec iter_load filename container =
  let label = new WX_button.with_label container filename [] in
  try
    let filename = try
        Utils.find_in_path !path filename
      with Not_found -> 
          Utils.find_in_path !path (filename^".tex")
    in
    label#set_action (fun () ->
        Printf.printf "Filename <%s> at %d" filename 0;print_newline ();
    );
    let inc = open_in filename in
    let s = Utils.read_string inc in
    close_in inc;
    let rec iter_search pos list = 
      try
        let newpos = Str.search_forward regexp s pos in
        let keyword = Str.matched_group 1 s in
        iter_search (newpos+String.length keyword) (
          (keyword,Str.matched_group 2 s,newpos)::list)
      with
        _ -> List.rev list
    in
    let list =  iter_search 0 [] in
    if list = [] then leaf 0 label#contained else
    let tree2 = new WX_tree.t container [] in
    tree2#set_desc (List.map (
        fun (keyword,name,pos) -> 
          match keyword with
            "input" -> iter_load name tree2#container
          | _ ->
              let (offset,prefix) =
                match keyword with
                  "section" -> 0, "s:"
                | "subsection" -> 10, "ss:"
                | "subsubsection" -> 20, "sss:"
                | _ -> 30,""
              in
              leaf offset (
                  let button = new WX_button.with_label tree2#container (
                      prefix^name) [] 
                  in
                  button#set_action (fun () ->
                      Printf.printf "Filename %s at %d" filename pos;
                      print_newline ();
                  );
                  button#contained
              )
      ) list);
    branch true label#contained tree2#contained
  with
    _ -> 
      leaf 0 label#contained


let _ =
  path := [Filename.dirname Sys.argv.(1)];
  tree#set_desc [iter_load Sys.argv.(1) tree#container];
  top#container_add tree#contained;
  top#show;
  loop ()