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

val default_font : string ref
val default_foreground : string ref
class ledit :
  string ->
  object
    inherit Label.label 
    val mutable edit : bool
    val mutable locked : bool

    
    method edit_set_string : unit
    method inverse : unit
  end
val make : string -> ledit
val desc : #ledit -> Gwml.wob_desc