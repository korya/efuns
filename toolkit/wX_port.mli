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
  object
    inherit WX_object.t
    
    val mutable widget : WX_types.contained option
      method container_add : WX_types.contained -> unit
      
  end
