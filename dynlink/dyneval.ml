(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Options

let debug_load = define_option ["debug_load"] "" bool_option false

let load_path = ref []
  
let libraries = define_option ["libraries_modules"] 
  "<libraries_modules> is a list containing, for each library, the modules
  which are contained in it."
    (list_option (tuple2_option (filename_option, smalllist_option string_option)))
  ["stdlib.cma", ["Parsing"]]

let rec load_module mod_name =
  if !!debug_load then
    Log.printf "load_module %s\n" mod_name;
  let filename = (String.uncapitalize mod_name) ^ ".cmo" in
  try
    let filename = Utils.find_in_path !load_path filename in
    if !!debug_load then
      Log.printf "loading file %s\n" filename;
    load_file filename
  with
    Not_found -> 
      if !!debug_load then
        Log.printf "load_module Not_found %s\n" mod_name;
      try
        List.iter (fun (filename, list) ->
            if List.mem mod_name list then
              let mod_name = ":" ^ mod_name in
              try
                let filename = Utils.find_in_path !load_path filename in
                load_file (filename ^ mod_name);
                raise Exit
              with
                Not_found -> failwith ("Can't find file "^filename)
              | Sys_error _ ->
          (* Should only append using the bytecode dynlink library
                  which does not understand the syntax lib:mod *)
                  Log.printf "load_module Sys_error %s\n" mod_name;
                  load_file filename;
                  raise Exit
        ) !!libraries;
        failwith ("Can't find file "^filename);        
      with
        Exit -> 
          Log.printf "load_module Exit %s\n" mod_name

          
and load_file filename =
  if !!debug_load then
    Log.printf "load_file %s\n" filename;
  try
    Dynlink.loadfile filename;
  with
    Dynlink.Error (Dynlink.Unavailable_unit mod_name) -> 
      load_interface mod_name;
      load_file filename
  | Dynlink.Error (
    Dynlink.Linking_error (_, Dynlink.Undefined_global mod_name)) ->
      load_module mod_name;
      load_file filename

and load_interface mod_name =
  if !!debug_load then
    Log.printf "load_interface: %s\n" mod_name;
  try
    Dynlink.add_interfaces [mod_name] !load_path; ()
  with
    Not_found ->
      failwith (Printf.sprintf "No interface for %s" mod_name)

let load mod_name =
  if !!debug_load then
    Log.printf "load %s\n" mod_name;
  try
    load_module mod_name
  with
    Dynlink.Error e -> 
      Printf.printf "Dynlink error: %s" (Dynlink.error_message e);
      print_newline () 
  | e ->
      Printf.printf "Dynlink runtime error: %s" (Printexc.to_string e);
      print_newline () 

let eval s = failwith "Dyneval.eval not implemented"

let compile f =
  let _ = Sys.command (Printf.sprintf "ocamlc %s -c %s"
      (List.fold_left
        (fun s d -> Printf.sprintf "%s -I %s" s d) "" !load_path)
      f)
  in ()
  
let init auth =
  Dynlink.init ();
  Dynlink.allow_unsafe_modules auth
