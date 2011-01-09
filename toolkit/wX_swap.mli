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
    
    val mutable tab : WX_types.contained array
    val mutable visible : int
    method container_add : WX_types.contained -> int
    method container_add_s : WX_types.contained list -> unit
    method set_visible : int -> unit
  end
