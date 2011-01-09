(***********************************************************************)
(*                                                                     *)
(*                             ____________                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Format
open Emitcode
(* Tables for numbering objects *)

type 'a numtable =
  { num_cnt: int;               (* The next number *)
    num_tbl: ('a, int) Tbl.t } (* The table of already numbered objects *)

let empty_numtable = { num_cnt = 0; num_tbl = Tbl.empty }

let find_numtable nt key =
  Tbl.find key nt.num_tbl

let enter_numtable nt key =
  let n = !nt.num_cnt in
  nt := { num_cnt = n + 1; num_tbl = Tbl.add key n !nt.num_tbl };
  n

open Predef
let global_map = ref empty_numtable
let map () =
  let globals =   
    (Interp.exceptions()) @
      (List.map (fun (name,_) ->
          Ident.create_persistent name
      ) Interp.globals_map) in
  let rec iter list =
    match list with
      [] -> ()
    | id :: tail ->
        let _ = enter_numtable global_map id in
        iter tail
  in
  iter globals;
  
(*
  List.iter (fun id ->      
      let n = find_numtable !global_map id in
      print_string "Find after put "; Ident.print id;
      Printf.printf " at %d"  n;
      print_newline ();
  ) globals;
  *)
  (Obj.magic !global_map : Symtable.global_map)
  
  
let install_modules reloc = 
  List.iter (fun (r,_) ->
      match r with
        Reloc_getglobal id when Ident.persistent id -> 
          begin
            try
              let i = find_numtable !global_map id in
              Interp.install_module i
            with
              _ -> ()
          end
      | _ -> ()
  ) reloc

external unsafe_blit : string -> int -> string -> int -> int -> unit
= "blit_string" "noalloc"

let alloc_code code code_size = 
  let c = String.create code_size in
  unsafe_blit code 0 c 0 code_size;
  Meta.static_free code;
  c
  
let free_code code = ()
  