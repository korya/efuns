(***********************************************************************)
(*                                                                     *)
(*                             Xlib                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projets Para/SOR, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(*
  Ressources management. Quite simplified.
*)

type 'a t

(* create a new database *)
val create: unit -> 'a t
val print: 'a t -> ('a -> string) -> unit

(* [load]: load a file into the database, 
   [save]: save the database in a file *)
val load: string t -> string -> unit
val safe_load: string t -> string -> unit
val save: string t -> string -> unit

(* [get]: look for a resource from the database, 
   [set]: modify a ressource *)
val get: 'a t -> string list -> 'a
val safe_get: 'a t -> string list -> 'a -> 'a
val set: 'a t -> string list -> 'a -> unit

val rget: 'a t -> string list -> string -> 'a
val safe_rget: 'a t -> string list -> string -> 'a -> 'a

val xdefaults : Xtypes.display -> string -> string t
