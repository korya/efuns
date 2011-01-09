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

type sound

val use_sound : bool Options.option_record
val use_local_player: bool Options.option_record
  
val sound_load : string -> string -> sound
val sound_unload : sound -> unit
val sound_play : sound -> unit
val file_play : string -> unit
val reconnect : string -> unit
val init_sound : string -> unit
  