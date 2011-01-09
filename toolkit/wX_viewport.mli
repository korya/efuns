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
  WX_adjust.t ->
  WX_adjust.t ->
  WX_types.base_attributes list ->
  object
    val adx : WX_adjust.t
    val ady : WX_adjust.t
    val mutable child : WX_types.contained option
    val view : Xtypes.geometry

      
        inherit WX_object.t
    method container_add : WX_types.contained -> unit
    method size_allocate_childs : unit
    method update_x_adjustement : unit -> unit
    method update_y_adjustement : unit -> unit
  end
