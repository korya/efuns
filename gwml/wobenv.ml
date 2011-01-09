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

open Obj
  
type 'a var = int
module Vars = Map.Make (struct  
      type t = int
      let compare = compare end)
  
type t = Obj.t ref Vars.t ref
type env = Obj.t Vars.t ref

let vars = ref 0
let new_var () = incr vars; !vars
let empty () = ref Vars.empty
let get t var = Obj.magic !(Vars.find var !t) 
let set vars var value =
  let value = repr value in
  try
    let r = Vars.find var !vars in
    r := value;
  with
    Not_found -> 
      vars := Vars.add var (ref value) !vars

let sget t var value = 
  try get t var with _ -> set t var value; value
      
let remove vars var =
  vars := Vars.remove var !vars
  
let empty_env () = ref Vars.empty
let get_env t var = Obj.magic (Vars.find var !t) 
let set_env vars var value =
  let value = repr value in
  (try
      let r = Vars.find var !vars in
      let r = Vars.remove var !vars in () 
    with
      Not_found -> ());
  vars := Vars.add var value !vars
  
let copy_env vars = ref !vars
      