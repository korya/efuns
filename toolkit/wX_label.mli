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
type options =
  | Justification of WX_types.justified
  | String of string
  | Font of string

class orig :
  WX_types.container ->
  string ->
  WX_types.base_attributes list ->
  object

    inherit WX_object.t
    val mutable cursor_x : int
      
    val mutable cursor_x : int
    val cursor : string
    val mutable display_cursor : bool
    val mutable font : WX_types.font
    val mutable justified : WX_types.justified
    val mutable string : string    
    
    method string : string
    method set_string : string -> unit
    method set_font : string -> unit
    method set_justification : WX_types.justified -> unit
    method set_options : options list -> unit
  end


class t :
  WX_types.container ->
  string ->
  WX_types.base_attributes list ->
  object

    inherit WX_object.t    
    
    method string : string
    method set_string : string -> unit
    method set_font : string -> unit
    method set_justification : WX_types.justified -> unit
    method set_options : options list -> unit
  end
