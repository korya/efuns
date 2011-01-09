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
val inverse : WX_types.bar_desc -> WX_types.bar_desc
class t :
  WX_types.container ->
  WX_types.bar_desc ->
  WX_types.base_attributes list ->
  object
    
    inherit WX_object.t
    
    val mutable bar : WX_bar.v
    val mutable labels : WX_types.contained array
    val mutable swap : WX_swap.t
    val mutable topbar : WX_bar.v
    val mutable visible : int
    method container_add : string * WX_types.contained -> int
    method container_add_s : (string * WX_types.contained) list -> unit
    method set_visible : int -> unit

end

class v : 
  WX_types.container ->
  WX_types.base_attributes list -> t

class h :
  WX_types.container ->
  WX_types.base_attributes list -> t
