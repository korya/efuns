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
  WX_types.container ->
  WX_types.base_attributes list ->
  object
    
    inherit WX_object.t
    
    val mutable wait_release : bool
    val mutable widget : WX_types.contained option
    val mutable old_relief : WX_types.relief    
    val mutable action : unit -> unit
    val mutable activated : bool
    method action : unit -> unit
    method activate : unit
    method container_add : WX_types.contained -> unit
    method desactivate : unit
    method set_action : (unit -> unit) -> unit
    method set_wait_release : bool -> unit
    method set_buttons : int list -> unit
end

class t :
  WX_types.container ->
  WX_types.base_attributes list ->
  object
    
    inherit WX_object.t
    
    method action : unit -> unit
    method activate : unit
    method container_add : WX_types.contained -> unit
    method desactivate : unit
    method set_action : (unit -> unit) -> unit
    method set_wait_release : bool -> unit
    
    method set_buttons : int list -> unit
end

class orig_with_label :
  WX_types.container ->
  string ->
  WX_types.base_attributes list -> 
  object
    inherit orig
    
    method set_string : string -> unit
    val mutable label : WX_label.t
    method label : WX_label.t
      end

class with_label :
  WX_types.container ->
  string ->
  WX_types.base_attributes list -> 
  object
    inherit t
    
    method set_string : string -> unit
      method label : WX_label.t
      
end
