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

val wmaker_file : string Options.option_record
val wmaker_path : string list Options.option_record
val find_menu : string -> string
val init : unit -> unit
val wmaker_menu : string -> Stdconfig.menu
val load_soundset : string -> unit
val install_theme : string -> unit
val menu : 'a -> Stdconfig.menu
