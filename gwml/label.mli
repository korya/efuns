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

class label :
  string ->
  object
    inherit Wob.wob_base
    
    val mutable justified : Gwml.justified
    val mutable string : string
    val mutable max_width : int
      
    method set_max_width : int -> unit
    method set_justified : Gwml.justified -> unit
    method set_string : string -> unit
    method string : string
end
val make : string -> label
val desc :
  #label ->  Gwml.wob_desc
  