{
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
  
  (* read the menu associated with WindowMaker ... 
  For now, we don't treat the #include directive, which should clearly be 
  handled by a generic preprocessor.
*)
  open Stddeco
open Gwml
open Options
open Themes
open Stdconfig
open Genlex

let wmaker_file = define_option ["wmaker_file"] "" filename_option "menu" 
let wmaker_path = define_option ["wmaker_path"] "" path_option []
  
type menu_tokens =
  STRING of string
| MENU | END
| EXEC of string
| LEGAL_PANEL
| INFO_PANEL
| WORKSPACE_MENU
| EXIT
| RESTART of string
| REFRESH
| EOF
| ARRANGE_ICONS
| SHUTDOWN
| SHOW_ALL
| HIDE_OTHERS
| SAVE_SESSION
| CLEAR_SESSION
| OPEN_MENU of string list
| WS_BACK of string

let maxstr = 200
let strbuf = String.create maxstr
let strlen = ref 0
  
  }



rule menu = parse
    "/*" { comment lexbuf }
  | ' ' | '\n' | '\t' { menu lexbuf }
  | '"' { strlen := 0;  STRING (string lexbuf) }
  | '\'' { strlen := 0; STRING (string2 lexbuf) }
  | '#' { strlen := 0; ignore (end_of_line lexbuf); menu lexbuf }
  | "MENU" { MENU }
  | "END" { END }
  | "EXEC" { strlen := 0; EXEC (end_of_line lexbuf)  }
  | "SHEXEC" { strlen := 0; EXEC (end_of_line lexbuf)  }
  | "RESTART" { strlen := 0; RESTART (end_of_line lexbuf)  }
  | "INFO_PANEL" { INFO_PANEL }
  | "LEGAL_PANEL" { LEGAL_PANEL }
  | "WORKSPACE_MENU" { WORKSPACE_MENU }
  | "EXIT" { EXIT }
  | "REFRESH" { REFRESH }
  | "ARRANGE_ICONS" { ARRANGE_ICONS }
  | "SHUTDOWN" { SHUTDOWN }
  | "SHOW_ALL" { SHOW_ALL }
  | "HIDE_OTHERS" { HIDE_OTHERS }
  | "SAVE_SESSION" { SAVE_SESSION }
  | "CLEAR_SESSION" { CLEAR_SESSION }
  | "OPEN_MENU"  { strlen := 0; OPEN_MENU (begin_args lexbuf []) }
  | "WS_BACK"  { WS_BACK (end_of_line lexbuf) }
  | eof { EOF }
  | _ { failwith "Syntax error in WindowMaker menu file" }


and end_of_line = parse
    ' ' | '\t' { end_of_line lexbuf }
  |  '\n' | eof  { "" }
  | _ { 
      strbuf.[!strlen] <- Lexing.lexeme_char lexbuf 0; 
      incr strlen; 
      if !strlen = maxstr then
        failwith "end_of_line too long in WindowMaker menu file";
      in_end_of_line lexbuf }

and in_end_of_line = parse
  | '\n' | eof  { String.sub strbuf 0 !strlen }
  | [ ^ '\n'] { 
      strbuf.[!strlen] <- Lexing.lexeme_char lexbuf 0; 
      incr strlen; 
      if !strlen = maxstr then
        failwith "end_of_line too long in WindowMaker menu file";
      in_end_of_line lexbuf }

and comment = parse
    "*/" { menu lexbuf }
  | eof { failwith "EOF in comment in WindowMaker menu file" }
  | '*' { comment lexbuf } 
  | [^ '*']+ {  comment lexbuf }

and string = parse 
    '"' { String.sub strbuf 0 !strlen }
  | eof { failwith "EOF in string in WindowMaker menu file" }
  | [^ '"']   { 
      strbuf.[!strlen] <- Lexing.lexeme_char lexbuf 0; 
      incr strlen; 
      if !strlen = maxstr then
        failwith "string too long in WindowMaker menu file";
      string lexbuf }

and string2 = parse
    '\'' { String.sub strbuf 0 !strlen }
  | eof { failwith "EOF in string in WindowMaker menu file" }
  | [^ '\'']   { 
      strbuf.[!strlen] <- Lexing.lexeme_char lexbuf 0; 
      incr strlen; 
      if !strlen = maxstr then
        failwith "string too long in WindowMaker menu file";
      string2 lexbuf }

and begin_args = parse
    '\n'| eof { fun args -> List.rev args }
  | ' ' | '\t' { begin_args lexbuf }
  | '\'' { fun args -> 
        strlen := 0; begin_args lexbuf ((string2 lexbuf)::args) }
  | '"' { fun args -> 
        strlen := 0; begin_args lexbuf ((string lexbuf)::args) }
  | [^ '"' '\'' ' ' '\t' '\n']  { 
      strlen := 1;
      strbuf.[0] <- Lexing.lexeme_char lexbuf 0; 
      end_args lexbuf }

and end_args = parse
    ' ' | '\t' { fun args -> 
        begin_args lexbuf ((String.sub strbuf 0 !strlen) :: args) }
  | '\n' | eof { fun args -> 
        List.rev ((String.sub strbuf 0 !strlen) :: args) }
  | [^ ' ' '\t' '\n' ]   { 
      strbuf.[!strlen] <- Lexing.lexeme_char lexbuf 0; 
      incr strlen; 
      if !strlen = maxstr then
        failwith "arg too long in WindowMaker menu file";
      end_args lexbuf }  

and wmaker = parse
  | ' ' | '\n' | '\t' { wmaker lexbuf }
  | '"' { strlen := 0; Some (String (string lexbuf)) }
  | '\'' { strlen := 0; Some (String (string2 lexbuf)) }
  | "(" { Some (Kwd "(") }
  | ")" { Some (Kwd ")") }
  | "{" { Some (Kwd "{") }
  | "}" { Some (Kwd "}") }
  | "," { Some (Kwd ",") }
  | ";" { Some (Kwd ";") }
  | "=" { Some (Kwd "=") }
  | [^ '"' '\'' ' ' '\n' '\t' '(' ')' '{' '}' ',' ';']+ { 
      Some (Ident(Lexing.lexeme lexbuf)) }
  | eof { None }

{
(* Sorry, default is french :) *)
  
let lang = ref (try Sys.getenv "LANG" with _ -> "fr")
  
let _ = 
  if !!smart_install then begin
      add_to_path_check wmaker_path "/usr/share/WindowMaker";
      add_to_path_check wmaker_path "/usr/local/share/WindowMaker";
      add_to_path_check wmaker_path 
        (Filename.concat Utils.homedir "GNUstep/Library/WindowMaker");
    end
  
let find_menu name =
  try Utils.find_in_path !!wmaker_path (name ^ "." ^ !lang)
  with _ -> try Utils.find_in_path !!wmaker_path name with _ -> name

type menu = (string * action) list
and action = 
  Menu of menu
| Fun of (Gwml.wob -> unit)
| AMenu of (unit -> Stdconfig.menu)

let load_menu = ref (fun filename () -> [])

let fork_exec cmd = 
  let pid = Concur.Thread.fork () in
  if pid = 0 then begin
      Printf.printf "Commande:";
      for i = 0 to Array.length cmd - 1 do
        Printf.printf "%s " cmd.(i)
      done;
      print_newline ();
      Unix.execvp cmd.(0) cmd
    end
  
let cmd_on_file cmd file w =
  match cmd with
    None -> Stdconfig.commandw file w
  | Some cmd ->
      let cmd = Array.of_list (cmd @ [file]) in
      fork_exec cmd

type value = 
  Value of string
| List of value list

let install_theme_fwd = ref (fun name -> ())
  
let rec make_menu noext dirs list =
  match list with
    [menu_file] -> 
      !load_menu menu_file 
  | "|" :: _ -> Printf.printf "PIPE Menu"; print_newline (); 
      (fun _ -> [])
  | "-noext" :: tail -> make_menu true dirs tail
  | "WITH" :: "setstyle" :: _ ->
      end_make_menu (fun file w -> 

          try
            !install_theme_fwd file
          with e -> 
              Printf.printf "%s" (Utils.printexn e);
              print_newline ();
              raise e
      ) noext dirs
  | "WITH" :: "wmsetbg" :: "-u" :: tail ->
      end_make_menu (cmd_on_file (Some ("wmsetbg" :: tail))) noext dirs
  | "WITH" :: tail -> end_make_menu (cmd_on_file (Some tail)) noext dirs
  | "THEMES_DIR" :: tail -> 
      let theme_path = List.map (fun s ->
            Filename.concat s "Themes"
        ) !!wmaker_path in
      make_menu noext (theme_path @ dirs) tail
  | "BACKGROUNDS_DIR" :: tail -> 
      make_menu noext ("/usr/share/WindowMaker/Backgrounds" :: 
        (Filename.concat Utils.homedir "GNUstep/Library/WindowMaker/Backgrounds") ::
        dirs) tail
  | dir :: tail -> make_menu noext (dir :: dirs) tail
  | [] -> end_make_menu (cmd_on_file None) noext dirs

and end_make_menu cmd_on_file noext dirs = fun _ -> 
    let files = 
      List.flatten (List.map (fun dir -> 
            List.map (fun file ->
                file, Filename.concat dir file
            ) (Utils.list_dir_normal dir)) dirs) in
    let files = if noext then
        List.map (fun (name, file) -> 
            ((try Filename.chop_extension name with _ -> name), file)
        ) files else files in
    List.map (fun (name, file) ->
        if Utils.is_directory file && 
        (* this is REALLY bad. we should add an arg to check this ... *)
          (let len = String.length file in
            len < 8 || (String.sub file (len-7) 7 <> ".themed")
          ) then
          name, [submenu_item], ActiveMenu 
            (fun w -> end_make_menu cmd_on_file noext [file] ())
        else
          name, [], Function (cmd_on_file file)) files
    
let make_exec cmd = Stdconfig.commandw (cmd ^ " &")
let make_exit sw = Wob.exit_gwml ()
let make_restart cmd sw = 
  if cmd = "" then Wob.restart () else begin
      Wob.restart_cmd := [| cmd |]; Wob.restart ()
    end
    
let add_item name cmd items = (name,cmd)::items
  
let rec parse_top menus menu_name prev_items = parser
    [< 'STRING name; m = parse_action menus menu_name prev_items name >] -> 
    m
|   [< >] -> 
    (menu_name, Menu (List.rev prev_items))
  
and parse_action menus menu_name prev_items name = parser
|   [< 'END; s >] ->
    if name <> menu_name then failwith "Menus names clash";
    begin
      match menus with
        [] -> assert false
      | (menu_name, items) :: menus ->
          parse_top menus menu_name (add_item name (Menu 
              (List.rev prev_items)) items) s
    end
| [< 'MENU; s >] -> parse_top ((menu_name, prev_items) :: menus) name [] s
| [< 'EXEC cmd; s >] -> parse_top menus menu_name 
      (add_item name (Fun (make_exec cmd)) prev_items) s
| [< 'INFO_PANEL; s >] -> parse_top menus menu_name prev_items s
| [< 'LEGAL_PANEL; s >] -> parse_top menus menu_name prev_items s
| [< 'WORKSPACE_MENU; s >] -> parse_top menus menu_name prev_items s
| [< 'RESTART wm; s >] -> parse_top menus menu_name 
      (add_item name (Fun (make_restart wm)) prev_items) s
| [< 'EXIT; s >] -> parse_top menus menu_name 
      (add_item name (Fun make_exit) prev_items) s
| [< 'REFRESH; s >] -> parse_top menus menu_name prev_items s
| [< 'ARRANGE_ICONS; s >] -> parse_top menus menu_name prev_items s
| [< 'SHUTDOWN; s >] -> parse_top menus menu_name prev_items s
| [< 'SHOW_ALL; s >] -> parse_top menus menu_name prev_items s
| [< 'HIDE_OTHERS; s >] -> parse_top menus menu_name prev_items s
| [< 'SAVE_SESSION; s >] -> parse_top menus menu_name prev_items s
| [< 'CLEAR_SESSION; s >] -> parse_top menus menu_name prev_items s
| [< 'OPEN_MENU menu; s >] -> 
    parse_top menus menu_name 
      (add_item name (AMenu (make_menu false [] menu)) prev_items) s
| [< 'WS_BACK color; s >] -> parse_top menus menu_name prev_items s    
  
let parse_menu lexbuf =
  let stream = Stream.from (fun n -> 
        match menu lexbuf with
          EOF -> None
        | t ->  
            Some t) in
  parse_top [] "WMAKER_MENU" [] stream
        
let open_menu name =
  let filename = find_menu name in
  Cpp.path := !!wmaker_path;
  let lexbuf = Cpp.preprocess filename in
  let m = 
    try parse_menu lexbuf with e -> 
        let location = Cpp.abort () in
        Printf.printf "%s :" location;
        raise e in
  m

let init () = ()

let rec make_menu menu () =
  List.map (fun (name, action) ->
      match action with
        Menu m -> name, [submenu_item], ActiveMenu (
            fun w -> make_menu m ())
      | Fun f -> name, [], Function f
      | AMenu f -> name, [submenu_item], 
          ActiveMenu (fun sw -> f ())
  ) menu
  
let wmaker_menu name =
  try
    let m = open_menu name in
    match m with
    | _, Menu [ _, Menu menu] -> make_menu menu ()
    | _, Menu menu -> make_menu menu ()
    | _ -> []
  with e -> 
      Printf.printf "Wmaker: %s" (Utils.printexn e);
      print_newline ();
      []

let _ = load_menu := (fun filename unit -> wmaker_menu filename)
  
open Genlex
  
let wmaker_lexer = make_lexer [ "=" ; "(" ; ")"; "{"; "}"; ";"; "," ]
  
let rec parse_module = parser
| [< 'Kwd "{"; v = parse_values; eof = parse_module >] -> 
    v :: eof
| [< >] -> []

and parse_values = parser
| [< 'Kwd "}" >] -> []
| [< id = parse_id; 'Kwd "="; v = parse_value; 'Kwd ";"; eov = parse_values >] ->
    (id,v) :: eov
    
and parse_value = parser
| [< 'Ident s >] -> Value s
| [< 'String s >] -> Value s
| [< 'Int i >] -> Value (string_of_int i)
| [< 'Float f >] -> Value (string_of_float f)
| [< 'Char c >] -> Value (let s = String.create 1 in s.[0] <- c; s) 
| [< 'Kwd "("; v = parse_list >] -> List v
    
and parse_id = parser
    [< 'Ident s >] -> s
|   [< 'String s >] -> s

and parse_list = parser
    [< 'Kwd ","; v = parse_list >] -> v
|   [< v = parse_value; t = parse_list >] -> v :: t
|   [< 'Kwd ")" >] -> []
    
let load_options  filename =
  let ic = open_in filename in
  try
    let lexbuf = Lexing.from_channel ic in
    let v = Stream.from (fun _ -> wmaker lexbuf) in
    try
      let m = parse_module v in
      close_in ic;
      m
    with e ->
        Printf.printf "At pos %d:" (Lexing.lexeme_start lexbuf);
        print_newline ();
        raise e
  with e -> close_in ic; raise e

let load_soundset name =
  let name = Filename.basename name in
  let path = List.map (fun f -> Filename.concat f "SoundSets") !!wmaker_path in
  let filename = Utils.find_in_path path name in
  let options = load_options filename in
  let soundpath = List.map (fun f -> Filename.concat f "Sounds") !!wmaker_path
  in
  List.iter (fun options ->
      List.iter (fun (name, value) ->
          try
            let value = match value with Value v -> v | _ -> raise Not_found in
            let option = match String.lowercase name with
                "appexit" -> exit_sound
              | "appstart" -> start_sound
              | "hide" -> hide_sound
              | "iconify" -> iconify_sound
              | "maximize" -> maximize_sound
              | "unmaximize" -> unmaximize_sound
              | "shade" -> shade_sound
              | "startup" -> startup_sound
              | "unhide" -> unhide_sound
              | "deiconify" -> deiconify_sound
              | "unshade" -> unshade_sound
              | _ -> raise Not_found
            in 
            let filename = Utils.find_in_path soundpath value in
            option =:= filename
          with _ -> ()
      ) options
  ) options

let find_image imagepath name =
  try
    Utils.find_in_path imagepath name
  with Not_found ->
  (* Can't always trust absolute paths *)
      Utils.find_in_path imagepath (Filename.basename name)
  
let set_back opt_bg opt_im imagepath value =
  match value with
    List [Value "spixmap"; Value name; _ ] ->
      let filename = find_image imagepath name in
      opt_im =:= ScaleImage filename
  | List [Value "tpixmap"; Value name; _ ] ->
      let filename = find_image imagepath name in
      opt_im =:= TileImage filename
  | List [Value "cpixmap"; Value name; _ ] ->
      let filename = find_image imagepath name in
      opt_im =:= TileImage filename
  | List [Value "mvgradient"; Value c1; Value _ ; Value c2 ] -> 
      let filename = Gradients.make_hgradient c1 c2 in
      opt_im =:= ScaleImage filename
  | List [Value "mhgradient"; Value c1; Value _ ; Value c2 ] -> 
      let filename = Gradients.make_hgradient c1 c2 in
      opt_im =:= ScaleImage filename
  | List [Value "mdgradient"; Value c1; Value _ ; Value c2 ] -> 
      let filename = Gradients.make_hgradient c1 c2 in
      opt_im =:= ScaleImage filename
  | List [Value "hgradient"; Value c1 ; Value c2 ] -> 
      let filename = Gradients.make_hgradient c1 c2 in
      opt_im =:= ScaleImage filename
  | List [Value "vgradient"; Value c1 ; Value c2 ] -> 
      let filename = Gradients.make_vgradient c1 c2 in
      opt_im =:= ScaleImage filename
  | List [Value "dgradient"; Value c1 ; Value c2 ] -> 
      let filename = Gradients.make_dgradient c1 c2 in
      opt_im =:= ScaleImage filename
  | _ -> ()
    
let install_theme name =
  Printf.printf "Install theme %s" name; print_newline ();
  Wob.reset_image_cache ();
  let path = List.map (fun f -> Filename.concat f "Themes") !!wmaker_path in
  let filename = Utils.find_in_path path name in
  let imagepath = List.map (fun s ->
        Filename.concat s "Pixmaps"
    ) !!wmaker_path in
  let imagepath = (List.map (fun s ->
          Filename.concat s "Backgrounds"
      ) !!wmaker_path) @ imagepath in
  let filename, imagepath = if Utils.is_directory filename then 
      Filename.concat filename "style", filename :: imagepath
    else filename, imagepath in
  let options = load_options filename in
  List.iter (fun options ->
      List.iter (fun (name,option) ->
          try
            match String.lowercase name, option with
              "workspaceback", back -> 
                set_back root_background root_image imagepath back
            | "ftitleback", back ->
                set_back active_background active_image imagepath back
            | "utitleback", back ->
                set_back title_background title_image imagepath back
            | "ftitlecolor", Value color ->
                active_foreground =:= color
            | "utitlecolor", Value color ->
                title_foreground =:= color
            | "menutitlecolor", Value color ->
                menu_title_foreground =:= color;
                iconMgr_title_foreground =:= color;
            | "menutextcolor", Value color ->
                menu_foreground =:= color;
                iconMgr_foreground =:= color;
            | "menutitleback", back ->
                set_back menu_title_background menu_title_image 
                  imagepath back;
                set_back iconMgr_title_background iconMgr_title_image 
                  imagepath back;
                
            | "menutextback", back ->
                set_back menu_background menu_image imagepath back;
                set_back iconMgr_background iconMgr_image imagepath back;
                
            | "windowtitlefont", Value font ->
                title_font =:= font;
                active_font =:= font;
            | "menutextfont", Value font ->
                menu_font =:= font;
                menu_hilite_font =:= font;
                iconMgr_font =:= font;
            | "menutitlefont", Value font ->
                menu_title_font =:= font;
                iconMgr_title_font =:= font;
            | "titlejustify", Value s ->
                title_justification =:= (match String.lowercase s with
                    "center" -> Center
                  | "left" -> Left
                  | "right" -> Right
                  | _ -> !!title_justification)
            | "highlighttextcolor", Value color ->
                menu_hilite_foreground =:= color;
                iconMgr_active_foreground =:= color;
            | "highlightcolor", Value color -> 
                menu_hilite_image =:= NoImage;
                menu_hilite_background =:= color;
                iconMgr_active_image =:= NoImage;
                iconMgr_active_background =:= color
            | _ -> ()
          with e ->
              Printf.printf "Error %s with option %s"
                (Utils.printexn e) name;
              print_newline ();
            (*
          IconTitleFont = "-*-helvetica-medium-r-normal-*-8-*-*-*-*-*-*-*";
          ClipTitleFont = "-*-helvetica-bold-r-normal-*-10-*-*-*-*-*-*-*";
          DisplayFont = "-*-helvetica-medium-r-normal-*-12-*-*-*-*-*-*-*";
          HighlightColor = white;
          HighlightTextColor = black;
          ClipTitleColor = black;
          CClipTitleColor = black;
          FTitleColor = white;
          PTitleColor = white;
          UTitleColor = gray20;
          FTitleBack = (tpixmap, Vader_titlebar_F.tif, black);
          PTitleBack = (tpixmap, Vader_titlebar_UF.tif, black);
          UTitleBack = (tpixmap, Vader_titlebar_UF.tif, black);
          MenuTitleColor = white;
          MenuTextColor = white;
          MenuDisabledColor = gray60;
          MenuTitleBack = (hgradient, "rgb:30/32/3e", black);
          MenuTextBack = (hgradient, "rgb:50/5a/5e", "rgb:20/2a/2e");
          IconBack = (spixmap, Vader_Button.tif, black);
          IconTitleColor = black;
          IconTitleBack = black;
              *)      
      ) options
  ) options;
  Array.iter (fun s ->
      let sw = s#wob in
      (* reset menus *)
      (!menu_manager)#reset sw;      
      (* reset the clients *)
      List.iter (fun (c,w) ->
          let tw = w.w_top in
          let focus = Wob.getenv tw focus_var in
          update_focus tw.w_oo focus
      ) (list_clients sw)) !screens;
  load_soundset name
  
let _ = install_theme_fwd := install_theme
  
let available_themes () = 
  let themes_path = List.map (fun s ->
        Filename.concat s "Themes"
    ) !!wmaker_path in
  List.flatten (List.map (fun dir -> Utils.list_dir_normal dir) themes_path)

let menu () =
  List.map (fun name ->
      name, [], Function (fun w -> install_theme name)) (available_themes ())

  
let menu _ = end_make_menu (fun file w -> 
      try
        install_theme file
      with e -> 
          Printf.printf "%s" (Utils.printexn e);
          print_newline ();
          raise e
  ) true (List.map (fun f -> Filename.concat f "Themes") !!wmaker_path) ()

  }