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

  
type sound = int

open Options
let use_sound = define_option ["use_sound"] 
  "<use_sound> is true if you want GwML to play sounds for some user
  actions." bool_option false

let use_local_player = define_option ["use_local_player"] 
  "<use_local_player> is true if you want GwML to use a program on your
    computer to play sounds instead of the built-in Esd library." 
  bool_option false

external c_sound_load : string -> string -> sound = "sound_load"
external sound_unload : sound -> unit = "sound_unload"
external c_sound_play : sound -> unit = "sound_play"
external c_file_play : string -> unit = "file_play"
external reconnect : string -> unit = "esd_reconnect"
external init_sound : string -> unit = "init_sound"
  
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
    
let file_play sound = 
  if !!use_sound then
    if !!use_local_player then
      ignore (Sys.command (Printf.sprintf "%s %s &" !!wav_player sound))
    else begin
        let pid = Concur.Thread.fork () in
        if pid > 0 then () else begin
            c_file_play sound;
            Unix.sleep 1;
            exit 0
          end
    end
    
let sound_load tag name = 
  let i = c_sound_load tag name in
  if i<0 then failwith "Error in sound_load";
  i

let sound_play sound =
  if !!use_sound then
    c_sound_play sound
  