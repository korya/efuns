(***********************************************************************)
(*                                                                     *)
(*                            AsmOpt                                   *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Asm

exception Found of string
  
let find_name program name star =
  let starmatch = if star then 
      let len = String.length name in
      fun global desc ->
        (if String.length global >= len &&  
            String.sub global 0 len = name then raise (Found global))
    else
    fun global desc -> 
      (if global = name then raise (Found global))
  in
  try
    Hashtbl.iter starmatch program.desc;
    Hashtbl.iter starmatch program.env;
    failwith (Printf.sprintf "%s%s not found" name 
        (if star then "*" else ""))
  with
    Found s -> s
          
let replace program name node = 
  let desc = try
      Hashtbl.find program.desc name
    with
      Not_found ->
        Hashtbl.find program.env name
  in
  desc.code.instrs <- node.instrs;
  desc.code.link <- node.link
  
  
  (* inline the body of <name> in program.
  For now, the body of name must be a single block, finishing with a 
  simple <ret>. No allocation point (ie no frame).
  *)

let peepholes = ref []
  
let rec apply macros program =
  match macros with
    [] -> ()
  | macro :: macros ->
      begin
        match macro with
        | Macro_replace ((name, star), node) ->
            let global = find_name program name star in
            replace program global node;
        | Macro_inline (name, star) ->
            let namelen = String.length name in
            Hashtbl.iter (fun global func ->
                if (star && String.length global >= namelen &&
                    String.sub global 0 namelen = name) || name = global then
                  func.fun_inline <- true
            ) program.desc
        | Macro_peephole (vars, name, orig, preds, dest) ->
            let ph = (name, vars, orig, preds, dest) in
            program.peepholes <-  ph :: program.peepholes;
            peepholes := ph :: !peepholes
      end;
      apply macros program