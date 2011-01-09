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
  WX_root.t ->
  WX_types.base_attributes list ->
  object
(*
    inherit WX_deleg.base 
    inherit WX_deleg.wx_object
  *)
      inherit WX_deleg.container_add
      inherit WX_deleg.wmtop
    
    method add_button : string -> (WX_button.t -> unit -> unit) -> unit
    method add_menu : string -> (string * WX_popup.action) array -> unit
    method add_separator : unit
      method configure : WX_types.base_attributes list -> unit
end
