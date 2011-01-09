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


type t
type 'a var

val new_var : unit -> 'a var
val empty : unit -> t
val get : t -> 'a var -> 'a
val set : t -> 'a var -> 'a -> unit
val sget : t -> 'a var -> 'a -> 'a
val remove : t -> 'a var -> unit
  
type env
val copy_env : env -> env
val empty_env : unit -> env
val get_env : env -> 'a var -> 'a
val set_env : env -> 'a var -> 'a -> unit