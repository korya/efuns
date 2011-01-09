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

open Lexers
open Parsers
  
let passwd =
  let ic = open_in "/etc/passwd" in
  let file = 
    try
      parse_passwd lexer_passwd (Lexing.from_channel ic) 
    with
      _ -> close_in ic; []
  in
  file

let decomp name =
  let lexbuf = Lexing.from_string name in
  parse_filename lexer_filename lexbuf

let homedir =
  try
    Sys.getenv "HOME"
  with
    Not_found -> ""
 
let homedirs = 
  List.fold_left  (fun dirs line ->
      match line with
        login::_::_::_::_::home::_ -> ("~"^login,home) :: dirs
      | _ -> dirs) ["~",homedir] passwd
      
let users = List.map fst homedirs
         
let string_to_filename filename =
  List.fold_left (fun filename (dir,repl) ->
      try
        let fin = Utils.check_prefix filename dir in
        repl ^ fin
      with
        Not_found -> filename) filename homedirs
  
  