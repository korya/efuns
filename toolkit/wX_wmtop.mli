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
val hostname : string

class orig :
  WX_root.t ->
  WX_types.base_attributes list ->
  object
    
    inherit WX_top.orig
    
    val mutable wm_class : string
    val mutable wm_class_app : string
    val mutable wm_command : string list
    val wm_hints : Xtypes.wm_hints
    val mutable wm_icon_name : string
    val mutable wm_name : string
    val wm_size_hints : Xtypes.wm_size_hints
    val mutable wm_transient_for : Xtypes.window

    method iconify : unit
    method deiconify : unit
    method setWM_CLASS : string -> string -> unit
    method setWM_ICON_NAME : string -> unit
    method setWM_NAME : string -> unit
    method setWM_TRANSIENT_FOR : WX_types.container -> unit
    method setWM_SIZE_HINTS : Xtypes.wm_size_hints -> unit
    method withdraw : unit
  end


class t :
  WX_root.t ->
  WX_types.base_attributes list ->
  object
    
    inherit WX_top.t

    method iconify : unit
    method deiconify : unit
    method setWM_CLASS : string -> string -> unit
    method setWM_ICON_NAME : string -> unit
    method setWM_NAME : string -> unit
    method setWM_TRANSIENT_FOR : WX_types.container -> unit
    method setWM_SIZE_HINTS : Xtypes.wm_size_hints -> unit
    method withdraw : unit
  end


