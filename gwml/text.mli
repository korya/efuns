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

class text :
  string array ->
  object
    inherit Wob.wob_base
    
    val mutable text : string array
      
    method size : unit    
    method set_text : string array -> unit
    method text : string array
end
val make : string array -> text
val desc :
  #text ->  Gwml.wob_desc
  