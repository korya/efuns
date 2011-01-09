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
class orig :
  WX_types.bar_desc ->
  WX_types.container ->
  WX_types.base_attributes list ->
  object
    
    inherit WX_object.t
    
    val mutable sens : WX_types.bar_desc
    val mutable wobs : WX_types.contained array
    
    method clear_items : unit
    method container_add : WX_types.contained -> unit
    method container_add_s : WX_types.contained list -> unit
    method container_insert : int -> WX_types.contained -> unit
    method container_remove : int -> unit
    method items : WX_types.contained array
    method nitems : int
    method set_items : WX_types.contained array -> unit
end

class t :
  WX_types.bar_desc ->
  WX_types.container ->
  WX_types.base_attributes list ->
  object
    
    inherit WX_object.t
    
    method clear_items : unit
    method container_add : WX_types.contained -> unit
    method container_add_s : WX_types.contained list -> unit
    method container_insert : int -> WX_types.contained -> unit
    method container_remove : int -> unit
    method items : WX_types.contained array
    method nitems : int
    method set_items : WX_types.contained array -> unit
end



class v :
  WX_types.container ->
  WX_types.base_attributes list -> t
  
class h :
  WX_types.container ->
  WX_types.base_attributes list -> t
