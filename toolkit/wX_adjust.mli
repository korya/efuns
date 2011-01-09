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
val v_total : int
type adjustement =
  { mutable v_pos: int;
    mutable v_inc: int;
    mutable v_page: int;
    mutable v_subjects: (unit -> unit) list }
class t :
  unit ->
  object
    val adj : adjustement
    val mutable validate : unit -> bool
    method add_subject : (unit -> unit) -> unit
    method down : unit
    method get_page : int -> int
    method get_pos : int -> int
    method page_down : unit
    method page_up : unit
    method set_inc : int -> unit
    method set_page : int -> int -> unit
    method set_params : int -> int -> int -> unit
    method set_pos : int -> int -> unit
    method set_validity : (unit -> bool) -> unit
    method up : unit
    method update_all : unit
    method update_x : Xtypes.geometry -> Xtypes.geometry -> unit
    method update_y : Xtypes.geometry -> Xtypes.geometry -> unit
  end
