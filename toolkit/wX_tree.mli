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
type desc = node list
and node =
  { mutable leaf_widget: WX_types.contained;
    mutable leaf_subtree: WX_types.contained option;
    mutable leaf_win: Xtypes.window;
    mutable leaf_closed: bool;
    mutable leaf_x: int;
    mutable leaf_y: int;
    mutable leaf_dx: int;
    mutable leaf_dy: int;
    mutable leaf_dy': int;
    mutable leaf_dx': int }
val leaf : int -> WX_types.contained -> node
val branch : bool -> WX_types.contained -> WX_types.contained -> node
val iter : (WX_types.contained -> 'a) -> node list -> unit
val iter_visible : (WX_types.contained -> unit) -> node list -> unit
val iter_size : node list -> int -> int -> int * int

class t :
  WX_types.container ->
  WX_types.base_attributes list ->
  object
    
    inherit WX_object.t
    
    val mutable desc : node list
    val xc_arrow : WX_types.cursor
    method destroy_desc : unit
    method hide_desc : unit
    method set_desc : node list -> unit
  end
