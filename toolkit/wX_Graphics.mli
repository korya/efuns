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
class t :
  WX_types.container ->
  WX_types.base_attributes list ->
  'a ->
  'b ->
  object
    
    inherit WX_object.t
    
    val mutable gv : XGraphics.view option
    method active : unit

  end
