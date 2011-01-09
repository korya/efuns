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

type sound = string

open Options
let use_sound = define_option ["use_sound"] 
  "<use_sound> is true if you want GwML to play sounds for some user
  actions." bool_option false

let use_local_player = define_option ["use_local_player"] 
  "<use_local_player> is true if you want GwML to use a program on your
    computer to play sounds instead of the NOT built-in Esd library." 
  bool_option false

let sound_load file tag = file
let sound_unload sound = ()
  
    
let wav_player = define_option ["wav_player"] 
  "<wav_player> is the command used to play wav files if
    <use_local_player> is true." string_option "wavp"
  
let path = try Utils.string_to_path (Sys.getenv "PATH") with Not_found -> []
  
let _ =
  if !!use_local_player then
    wav_player =:= (
      try Utils.find_in_path path !!wav_player with
        _ -> try Utils.find_in_path path "wavp" with
        _ -> try Utils.find_in_path path "wavplay" with _ -> "wavp")
    
let sound_play sound = 
  if !!use_local_player && !!use_sound then
    ignore (Sys.command (Printf.sprintf "%s %s &" !!wav_player sound))
    
let file_play string = sound_play string
let reconnect string = ()
let init_sound _ = ()
  