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
  WX_types.bar_desc ->
  WX_types.container ->
  WX_adjust.t ->
  WX_types.base_attributes list ->
  object
    
    inherit WX_object.t
    val adj : WX_adjust.t
    val mutable first : WX_types.contained
    val mutable second : WX_types.contained
    val mutable sens : WX_types.bar_desc
    val mutable separator : WX_types.contained
    val mutable step : int
    
    method move_separator : int -> int -> unit
    method set_second : WX_types.contained -> unit
    method set_separator : WX_types.contained -> unit
    method set_step : int -> unit
    method sens : WX_types.bar_desc
    method set_first : WX_types.contained -> unit
    method update_childs : unit -> unit
end

class separator :
  t ->
  < get_pos : int -> int; set_pos : int -> int -> 'a; .. > ->
  WX_types.base_attributes list ->
  object
    inherit WX_object.t
    val mutable mini_win : int
    val mutable parent : WX_types.container
    val sens : WX_types.bar_desc
    val xc_hand2 : WX_types.cursor
    
    method track_mouse : WX_types.handler
end
