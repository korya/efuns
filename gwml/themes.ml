(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Options
open Xtypes
open Gwml
open Stdconfig

let init () = ()

  (* This is an experiment of using Imlib to load images in the
titlebar ... This comes from some sawmill themes. *)
  (*
  let theme_directory = ref "/home/specialix/lefessan/src/blue-heart/"
let theme_title_filename = ref "A_b_title1.png"
    *)

let themes_path = define_option ["themes_path"] "" path_option []
let asthemes_path = define_option ["asthemes_path"] "" path_option []
let ethemes_path = define_option ["ethemes_path"] "" path_option []
  
let add_to_path_check path dir =
  if Sys.file_exists dir then add_to_path path dir
  
let _ = 
  if !!smart_install then begin
      (* Try to find all directories where Themes could be located on this system *)
      add_to_path_check asthemes_path "/usr/share/afterstep/desktops/themes";
      add_to_path_check asthemes_path "/usr/local/share/afterstep/desktops/themes";
      add_to_path_check asthemes_path (Filename.concat Utils.homedir "GNUstep/Library/Afterstep/desktops/themes");
      
      add_to_path_check ethemes_path "/usr/share/enlightenment/themes";
      add_to_path_check ethemes_path "/usr/local/share/enlightenment/themes";
    end
  
let available_themes () = 
  List.flatten (List.map (fun dir -> Utils.list_dir_normal dir) !!asthemes_path)

  (*
let theme = define_option ["theme"] "" string_option
    (try 
      match available_themes () with
        [] -> ""
      | file :: _ -> file
    with _ -> ""
  )
  *)

let setroot_cache = ref ""
let setroot_cmd = ref "wmsetbg"

let binpath = 
  List.map Utils.string_to_filename (Utils.string_to_path
  (try
      Sys.getenv "PATH"
    with _ -> "/bin:/usr/bin:/usr/local/bin:/usr/bin/X11:/usr/local/bin/X11"))

let root_pixmap = ref noPixmap
let pager_pixmap = ref noPixmap

let root_image_command = define_option ["root_image_command"] "" string_option ""
  
let set_root sw =
  let so = sw.w_oo in
  let sw = so#wob in
  let s = sw.w_screen.s_scr in
  
  if !!root_image_command <> "" then
    commandw (Printf.sprintf "(%s) &" !!root_image_command) sw
  else
  match !!root_image with
    NoImage ->
      if !!root_background <> "" then begin
          X.changeWindowAttributes display s.scr_root
            [CWBackPixel (color_make sw !!root_background)];
          X.clearArea display s.scr_root 0 0 0 0 false
        end
  | _ -> 
      let filename = match !!root_image with
          NoImage -> assert false
        | ScaleImage file -> file
        | TileImage file -> file
      in
      if !!use_imlib then
        ungrab_exec (fun _ ->
            try
              if not (Sys.file_exists filename) then raise Not_found;
              let im = Imager.image_load filename in
              if not (!root_pixmap == noPixmap) then
                Imager.pixmap_free !root_pixmap;
              let pix = match !!root_image with
                  TileImage _ -> 
                    Imager.image_pixmap im im.Imager.w im.Imager.h
                | ScaleImage _ ->
                    Imager.image_pixmap im s.scr_width s.scr_height
                | NoImage -> assert false
              in
              let pix = pix.Imager.pixmap in
              root_pixmap := pix;
              X.changeWindowAttributes display s.scr_root [CWBackPixmap pix];
              X.clearArea display s.scr_root 0 0 0 0 false;
              let pager = (!virtual_manager)#pager sw in
              let pix = Imager.image_pixmap im pager.pager_width 
                  pager.pager_height in
              let pix = pix.Imager.pixmap in
              X.changeWindowAttributes display pager.pager_window
                [CWBackPixmap pix];
              X.clearArea display pager.pager_window 0 0 0 0 false;
              (!virtual_manager)#update sw;
              if not (!pager_pixmap == noPixmap) then
                Imager.pixmap_free !pager_pixmap;
              pager_pixmap := pix;
              Imager.image_kill im;
            with   _ -> 
                X.changeWindowAttributes display s.scr_root
                  [CWBackPixel (color_make sw !!root_background)];
                X.clearArea display s.scr_root 0 0 0 0 true
        )
      else
        begin
      (* try to find the correct command on this system to set an image as
    root... *)
          if not (Sys.file_exists !setroot_cache) then begin
              try
          (* Enlightment version *)
                setroot_cache := Utils.find_in_path binpath "Esetroot";
                setroot_cmd := "Esetroot -scale"
              with Not_found ->
                  try
                (* WindowMaker version *)
                    setroot_cache := Utils.find_in_path binpath "wmsetbg";
                    setroot_cmd := "wmsetbg -u -t"
                  with Not_found ->
                      try
                    (* Xloadimage version *)
                        setroot_cache := Utils.find_in_path binpath "xsetbg";
                        setroot_cmd := "xsetbg -u -t"
                      with Not_found ->
                          try
                    (* xv version *)
                            setroot_cache := Utils.find_in_path binpath "xv";
                            setroot_cmd := "xv -quit -root"
                          with Not_found -> ()
            end;
          let _ = command (Printf.sprintf "%s %s &" !setroot_cmd filename)
          in ()
        end
        
let add_hook hooks hook = 
  hooks := hook :: !hooks
  
let _ =
  if not !Gwml_args.batch_mode then
  add_hook screen_opening_hooks (fun sw ->
        option_hook root_image (fun _ -> set_root sw);
      set_root sw)

  (* from an afterstep theme only ... *)
let set_background_theme name sw =
  let file = try
      Utils.find_in_path !!asthemes_path 
        (Filename.concat name ("background." ^ name))
    with _ -> "" in 
  if Sys.file_exists file then begin
      root_image =:= ScaleImage file;
      set_root sw
    end   
  
let themes_menu () =
  List.map (fun name ->
      name, [], Function (set_background_theme name)) (available_themes ())

module Esound = struct
    open Genlex
    
    let lexer = make_lexer [ "#include"; "<"; ">"; "__E_CFG_VERSION"; "BEGIN_SOUND";
        "END_SOUND" ]
    
    let rec parse = parser 
        [< 'Kwd "#include"; 'Kwd "<"; 'Ident id; 'Kwd ">"; s >] -> parse s
      | [< 'Kwd "BEGIN_SOUND"; sounds = parse_sounds >] -> sounds
      | [< >] -> []

    and parse_sounds = parser
        [< 'Kwd "END_SOUND" >] -> []
      | [< 'String s1; 'String s2; list = parse_sounds >] -> (s1,s2) :: list
    
    let read_esound filename =
      let ic = open_in filename in
      try
        let s = Stream.of_channel ic in
        let v = lexer s in
        (* close_in ic; *)
        v
     with e -> close_in ic; raise e

end

(* from an Enlightment theme only ... *)
let set_sound_theme theme =
  ()
  
let find_image image = 
  try
    Utils.find_in_path (!!asthemes_path @ !!ethemes_path @ !!themes_path) image
  with
    Not_found -> 
      Printf.printf "Unable t found %s" image;
      print_newline ();
      image