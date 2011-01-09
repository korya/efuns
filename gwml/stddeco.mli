(***********************************************************************)
(*                                                                     *)
(*                             GwML                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

val update_focus : Gwml.wob_desc -> bool -> unit
val name_update :
  Gwml.client_desc ->
  < set_string : string -> unit; wob : Gwml.wob; .. > ->
  Gwml.wob_event -> unit
val c_hook : Gwml.client_desc -> Top.top -> Gwml.wob_event -> unit
class c_ledit : Gwml.client_desc -> Ledit.ledit
val window_popup : Stdconfig.menu Options.option_record
val simple_window :
  Gwml.wob ->
  Gwml.client_desc -> Top.window_desc
val sticky_hook : < wob : Gwml.wob; .. > -> Gwml.wob_event -> unit
val no_deco :
  'a -> Gwml.client_desc -> Top.window_desc
val generic_window : string ->
  Gwml.wob ->
  Gwml.client_desc -> Top.window_desc

  val decorate : (string * (string * string) list) list Options.option_record
  
val assoc_option : (string * 'a Options.option_class) list ->
    (string * 'a) Options.option_class
val object_option :  (string * string) list Options.option_class
val bar_options :   (string *(string * string) list Options.option_class) list
val bar_option :    (string * (string * string) list) list 
  Options.option_class
val deco_options :     (string * (string * (string * string) list) list
    Options.option_class) list
val deco_option :     (string * (string * (string * string) list) list) list
    Options.option_class

type direction = North | East | West | South
  
val direction_option : direction Options.option_class

val set_temp_reduced_window :  Gwml.client_desc -> Gwml.wob -> direction * int -> unit
val get_reduced_window : Gwml.client_desc -> Gwml.wob -> direction * int
