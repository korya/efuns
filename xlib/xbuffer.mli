(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

val int_of_enum : 'a -> int
val enum_of_int : int -> 'a
val list_of_mask : int -> 'a
val mask_of_list : 'a list -> int
val setCard8 : string -> int -> int -> unit
val setCard16 : string -> int -> int -> unit
val setCard32 : string -> int -> int -> unit
val getCard8 : string -> int -> int
val getCard16 : string -> int -> int
val getCard32 : string -> int -> int
val setInt8 : string -> int -> int -> unit
val setInt16 : string -> int -> int -> unit
val setInt32 : string -> int -> int -> unit
val getInt8 : string -> int -> int
val getInt16 : string -> int -> int
val getInt32 : string -> int -> int
val setEnum8 : string -> int -> 'a -> unit
val setEnum16 : string -> int -> 'a -> unit
val setEnum32 : string -> int -> 'a -> unit
val getEnum8 : string -> int -> 'a
val getEnum16 : string -> int -> 'a
val getEnum32 : string -> int -> 'a
val string_blit : string -> int -> string -> int -> int -> unit
val setString : string -> int -> string -> unit
val getString : string -> int -> int -> string
val strLen : int -> int
val get_card32_list : string -> int -> int -> int list
val get_enum32_list : string -> int -> int -> 'a list
val set_int32_list : string -> int -> int list -> unit
val set_enum32_list : string -> int -> 'a list -> unit
val get_card32_array : string -> int -> int -> int array
val set_card32_array : string -> int -> int array -> unit

val newString : int -> string
val set_str_list : string -> int -> string list -> unit
val get_str_list : string -> int -> int -> string list
val setString16 : string -> int -> int array -> unit

val getXError : string -> Xtypes.errorEvent
val getXServerInfo : string -> Unix.file_descr -> int -> int -> int -> string
  -> Xtypes.display


val getWindow : string -> int -> Xtypes.window
val setWindow : string -> int -> Xtypes.window -> unit
val getFont : string -> int -> Xtypes.window
val setFont : string -> int -> Xtypes.font -> unit
val getColormap : string -> int -> Xtypes.colormap
val setColormap : string -> int -> Xtypes.colormap -> unit
val getAtom : string -> int -> Xtypes.atom
val setAtom : string -> int -> Xtypes.atom -> unit
val getTime : string -> int -> Xtypes.time
val setTime : string -> int -> Xtypes.time -> unit