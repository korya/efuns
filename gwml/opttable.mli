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

(* This table associates strings with wild chars( ?* ) to values *)

type 'a t
val create : unit -> 'a t
val find : 'a t -> string -> 'a
val add : 'a t -> string -> 'a -> unit
