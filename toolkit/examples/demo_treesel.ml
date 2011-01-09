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



open Xtypes
open WX_types
open WX_tree
open Unix

let _ =
  default_background := "gray51";
  default_foreground := "white"

let replay = ref false
  
(*********** taken from efuns/efuns/system.ml  *)

open Unix
open Concur
open ThreadUnix

let open_process cmd =
  let (in_read, in_write) = pipe() in
  let (out_read, out_write) = pipe() in
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  match fork() with
    0 ->
      if out_read <> stdin then begin
          dup2 out_read stdin; close out_read end;
      if in_write <> Unix.stdout ||  in_write <> Unix.stderr then begin
          if in_write <> Unix.stdout then dup2 in_write stdout;
          if in_write <> Unix.stderr then dup2 in_write stderr; 
          close in_write end;
      List.iter close [in_read;out_write];
      execvp cmd.(0) cmd;
      exit 127
  | pid -> 
      Unix.close out_read;
      Unix.close in_write;
      (pid,inchan, outchan)

let system cmd log end_action =
  let (pid,inc,outc) = open_process cmd in
  let ins = Unix.descr_of_in_channel inc in
  let tampon = String.create 1000 in
  Thread.add_reader ins
    (function () ->
        try
          let len = input inc tampon 0 1000 in
          if len = 0 then
            begin
              close_in inc;
              close_out outc;
              let pid,status = try waitpid [WNOHANG] pid with _ -> 
                    pid, WSTOPPED 9 in            
              begin
                match status with 
                  WEXITED s -> 
                    (try end_action () with _ -> ())
                | _ -> 
                    (try end_action () with _ -> ());
                    log "Broken pipe" 
              end;
              Thread.remove_reader ins (* Kill self *)
            end
          else
            log (String.sub tampon 0 len);
        with
          e -> 
            Printf.printf "exception";
            );
  let write str =
    output outc str 0 (String.length str);
    flush outc
  in
  pid,write
  
(*************************************************)

let argv = Array.to_list Sys.argv
let regexps = List.map (fun str -> str, 
      Str.regexp (Utils.glob_to_regexp str)) (
    match argv with
      program :: dirname :: strs -> strs
    | _ -> failwith "Usage: eplayer dirname")
let regexps = ["*.mp3",Str.regexp (Utils.glob_to_regexp "*.mp3")]
  
let dirname = ref (if Filename.is_relative Sys.argv.(1) then
      Filename.concat (Sys.getcwd ()) Sys.argv.(1) else Sys.argv.(1))
  
let display = new WX_display.t ""
let root = new WX_root.t display 0
let top = new WX_appli.t root [MinWidth 10; MinHeight 10; MaxHeight (root#height - 200)]
let notebook = new WX_notebook.h top#container []
let adx = new WX_adjust.t ()
let ady = new WX_adjust.t ()
let hbar = new WX_bar.h notebook#container []
let viewport = new WX_viewport.t hbar#container adx ady []
let scrollbar = new WX_scrollbar.v hbar#container ady []
let tree = new WX_tree.t viewport#container []

let current = ref 0
let stop _ = 
  if !current <> 0 then 
    (try 
        Unix.kill !current Sys.sigkill;
        let _,_ = waitpid [] !current in ()
      with _ -> ());
  current := 0

type file = {
    file_name  : string;
    mutable file_played : bool;
    file_label : WX_button.with_label;
  }

let play file = 
  stop ();
  let pid, write =
    system [| "mpg123"; file.file_name |] (fun _ -> ())
    (fun _ -> current := 0)
  in
  current := pid
  
let load file =
  match !button_event with
    1 -> play file
  | 2 ->
      file.file_played <- not file.file_played;
      file.file_label#label#configure
        (if file.file_played then 
          [Background "green"] else
          [Background "gray51"])
  | 3 -> ()
  | _ -> ()
      
let files = ref []
  
let rec play_all () =
  let n = ref 0 in
  List.iter (fun file ->
      try if file.file_played then (
            incr n;play file) with _ -> ()
  ) !files;
  if !n>0 && !replay then play_all ()
      
let add_file file = 
  files := file :: !files
      
let rec iter_load closed dirname basename container =
  try
    let dirname = Filename.concat dirname basename in
    let filenames = Sort.list (<=) (Utils.list_dir dirname) in
    let subdirs = List.fold_left (fun files filename ->
          if filename <> "." && filename <> ".." then
            let fullname = Filename.concat dirname filename in
            let stats = lstat fullname in
            if stats.st_kind = S_DIR then filename::files else
              files
          else files
      ) [] filenames in
    let file_lists = List.fold_left (fun lists (name,regexp) ->
          let newlist = List.rev (List.fold_left (fun files filename ->
                  if Str.string_match regexp filename 0 then filename :: files
                  else files
              ) [] filenames) in
          match newlist with
            [] -> lists
          | _ -> [name,List.rev newlist] @ lists
      ) [] regexps in
    (List.map (fun subdir ->
          let label = new WX_label.t container subdir [] in
          let tree  = new WX_tree.t container [] in
          tree#set_desc (iter_load true dirname subdir tree#container);
          branch true label#contained tree#contained
      ) subdirs) @ (
      match file_lists with
        [] -> []
      | [name,files] ->
          List.map (fun  filename ->
              let label = new WX_button.with_label container filename 
                  [IpadX 1; IpadY 1] 
              in
              let file = {
                  file_name = Filename.concat dirname filename;
                  file_label = label;
                  file_played = false;
                } in
              add_file file;
              label#set_buttons [1;2;3];
              label#set_action (fun () -> load file);
              leaf 0 label#contained
          ) (List.rev files);
      | _ ->
          List.map (fun (name,files) -> 
              let label = new WX_label.t container name [] in
              let tree  = new WX_tree.t container [] in
              tree#set_desc (List.map (fun filename ->
                    let label = new WX_button.with_label tree#container
                        filename [IpadX 1; IpadY 1] 
                    in
                    let file = {
                        file_name = Filename.concat dirname filename;
                        file_label = label;
                        file_played = false;
                      } in
                    add_file file;
                    label#set_buttons [1;2;3];
                    label#set_action (fun () -> 
                        load file);
                    leaf 0 label#contained
                ) (List.rev files));
              branch true label#contained tree#contained         
          ) file_lists
    )
  with
    _ -> []
      
open WX_filesel
  
let file_menu = [|
    "Open", (fun _ -> 
        let info = {
            filter = Filename.concat !dirname  "*";
            current_selection= !dirname;
            predicat = (fun _ -> true);
            action = (fun s -> 
                tree#destroy_desc;
                dirname := s;
                tree#set_desc (
                  iter_load false (Filename.dirname s) (Filename.basename s)
                  tree#container));
            cancel = (fun _ -> ());
          }    in
        let filesel = new WX_filesel.t root info [] in
        filesel#show
        );
    "Quit", (fun _ -> exit 0);
  |]
  
  (************************************** MIXER Stuff *)
open Unix
  
external mixer_init : file_descr -> int array = "mixer_init"
external mixer_label : int -> string * string = "mixer_label"
external get_volume : file_descr -> int -> int = "get_volume"  
external set_volume : file_descr -> int -> int -> int = "set_volume"  

let with_mixer f =
  let fd = openfile "/dev/mixer" [O_RDWR] 0 in
  try let x = f fd in close fd; x with e -> close fd; raise e

type mixer_device = {
    dev_id : int;
    dev_name : string;
    dev_label : string;
    dev_stereo : bool;
    dev_recorder : bool;
  }
      
let devices =      
  let tab = with_mixer mixer_init in
  let len = Array.length tab / 3 in
  Array.init len (fun i -> 
      let name, label = mixer_label tab.(i*3) in
      {
        dev_id = tab.(i*3);
        dev_name = name;
        dev_label = label;
        dev_stereo = tab.(i*3+1) = 1;
        dev_recorder = tab.(i*3+2) = 1;
      })

let mixer = new WX_bar.v notebook#container []
let levels =
  Array.map (fun dev ->
      let bar = new WX_bar.h mixer#container [] in
      let level = new WX_adjust.t () in
      let scale = new WX_scale.h bar#container level [MinWidth 300] in
      let label = new WX_label.t bar#container dev.dev_name [] in
      let value = new WX_label.t bar#container "   0 %" [] in
      let init = ref true in
      level#add_subject (fun () ->
          let v = level#get_pos 100 in
          value#set_string (Printf.sprintf " %3d %s" v "%");
          if not !init then
            let vol = v * 127 / 100 in
            let _ = with_mixer (fun fd ->
                  let old_vol = get_volume fd dev.dev_id in
                  let new_vol =
                    if dev.dev_stereo then
                      (vol * 256 + vol)
                    else
                      (old_vol land 0x7f00) lor vol
                  in
                  set_volume fd dev.dev_id new_vol);
            in ()
      );
      with_mixer (fun fd ->
          let v = (get_volume fd dev.dev_id) land 0x7f in
          level#set_pos v 128);
      init := false;
      bar#container_add_s [label#contained; scale#contained; value#contained];
      mixer#container_add bar#contained;
      level
  ) devices

let set_all i =
  Array.iter (fun level -> level#set_pos i 100) levels
let silence _ = set_all 0
let _ =
  let button = new WX_button.with_label mixer#container "Silence" [] in
  button#set_action silence;
  mixer#container_add button#contained
  
  (*****************************************************)
  
let _ =
  tree#set_desc (iter_load false (Filename.dirname Sys.argv.(1))
    (Filename.basename Sys.argv.(1))
    tree#container);
  hbar#configure [Bindings [
      Key(XK.xk_Prior,0),(fun _ -> ady#page_up);
      Key(XK.xk_Next,0),(fun _ -> ady#page_down);
      Key(XK.xk_Down,0),(fun _ -> ady#down);
      Key(XK.xk_Up,0),(fun _ -> ady#up);
    ]];
  top#configure [Bindings [
      GrabbedKey(XK.xk_q,mod1Mask),(fun _ -> 
          stop ();exit 0);
      GrabbedKey(XK.xk_m,mod1Mask),(fun _ -> notebook#set_visible 1);
      GrabbedKey(XK.xk_s,mod1Mask),stop;
      GrabbedKey(XK.xk_0,mod1Mask),(fun _ -> set_all 0);
      GrabbedKey(XK.xk_1,mod1Mask),(fun _ -> set_all 10);
      GrabbedKey(XK.xk_2,mod1Mask),(fun _ -> set_all 20);
      GrabbedKey(XK.xk_3,mod1Mask),(fun _ -> set_all 30);
      GrabbedKey(XK.xk_4,mod1Mask),(fun _ -> set_all 40);
      GrabbedKey(XK.xk_5,mod1Mask),(fun _ -> set_all 50);
      GrabbedKey(XK.xk_6,mod1Mask),(fun _ -> set_all 60);
      GrabbedKey(XK.xk_7,mod1Mask),(fun _ -> set_all 70);
      GrabbedKey(XK.xk_8,mod1Mask),(fun _ -> set_all 80);
      GrabbedKey(XK.xk_9,mod1Mask),(fun _ -> set_all 90);
      GrabbedKey(XK.xk_p,mod1Mask), play_all;
      ]];
  top#container_add notebook#contained;
  notebook#container_add_s ["MP3s", hbar#contained; "Mixer", mixer#contained];
  hbar#container_add_s [viewport#contained; scrollbar#contained];
  viewport#container_add tree#contained;
  top#setWM_NAME (Printf.sprintf "File player");
  top#add_menu "File" file_menu;
  top#add_button "Play" (fun _ -> play_all);
  top#add_button "Replay" (fun button () -> 
      replay := not !replay; 
      button#configure
        ( if !replay then [Background "green"] else [Background "gray51"])
  );
  ady#set_inc 10;
  top#show;
  loop ()