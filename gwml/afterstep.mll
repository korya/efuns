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

(* Load a theme from Afterstep *)
  open Wob  
  open Gwml
  open Options
  open Themes
  open Stdconfig
  open Stddeco
    
  type look_tokens = 
    STYLE of string list
  | FONT of string list
  | FORECOLOR of string list
  | BACKCOLOR of string list
  | MAXCOLORS of string list
  | INHERIT of string list
  | ENDSTYLE
  | BACKPIXMAP of string list
  | TEXTSTYLE of string list
  | BUTTONPIXMAP of string list
  | MARROWPIXMAP of string list
  | MENUPINON of string list
  | WINDOWFONT of string list
  | ICONFONT of string list
  | TITLEBUTTON of string list
  
  | DEFAULTSTYLE of string list
  | FWINDOWSTYLE of string list
  | UWINDOWSTYLE of string list
  | SWINDOWSTYLE of string list
  | MENUITEMSTYLE of string list
  | MENUTITLESTYLE of string list
  | MENUHILITESTYLE of string list
  | MENUSTIPPLESTYLE of string list  
    
  | EOF
    
let maxstr = 200
let strbuf = String.create maxstr
let strlen = ref 0

}

rule look = parse
  | '#' [^ '\n']*
  | ' ' | '\n' | '\t'  { look lexbuf }  
  
  | "MyStyle" { STYLE (args lexbuf 1) }
  | "Font"       { FONT (args lexbuf 1) }
  | "ForeColor"  { FORECOLOR (args lexbuf 1) }
  | "BackColor"  { BACKCOLOR (args lexbuf 1) }
  | "MaxColors"  { MAXCOLORS (args lexbuf 1) }
  | "Inherit"    { INHERIT (args lexbuf 1) }
  | "~MyStyle"   { ENDSTYLE }
  | "BackPixmap" { BACKPIXMAP (args lexbuf 2) }
  | "TextStyle"  { TEXTSTYLE (args lexbuf 1) }
  | "ButtonPixmap" { BUTTONPIXMAP (args lexbuf 1) }
  | "MArrowPixmap" { MARROWPIXMAP (args lexbuf 1) }
  | "MenuPinOn" { MENUPINON (args lexbuf 1) }
  | "WindowFont" { WINDOWFONT(args lexbuf 1) }
  | "IconFont"   { ICONFONT(args lexbuf 1) }
  | "TitleButtonStyle"  { end_of_line lexbuf; look lexbuf }
  | "TitleButtonSpacing"  { end_of_line lexbuf; look lexbuf }
  | "TitleButton" { TITLEBUTTON (args lexbuf 3) }
  
  
  | "DefaultStyle" { DEFAULTSTYLE (args lexbuf 1) }
  | "FWindowStyle" { FWINDOWSTYLE (args lexbuf 1) }
  | "UWindowStyle" { UWINDOWSTYLE (args lexbuf 1) }
  | "SWindowStyle" { SWINDOWSTYLE (args lexbuf 1) }
  | "MenuItemStyle" { MENUITEMSTYLE (args lexbuf 1) }
  | "MenuTitleStyle" { MENUTITLESTYLE (args lexbuf 1) }
  | "MenuHiliteStyle" { MENUHILITESTYLE (args lexbuf 1) }
  | "MenuStippleStyle" { MENUSTIPPLESTYLE (args lexbuf 1) }
    
    
  (* Values which are not parsed (nor used): *)
  | eof { EOF } 
  | _ { end_of_line lexbuf; look lexbuf }

and string = parse 
    '"' { String.sub strbuf 0 !strlen }
  | eof { failwith "EOF in string in WindowMaker menu file" }
  | _   { 
      strbuf.[!strlen] <- Lexing.lexeme_char lexbuf 0; 
      incr strlen; 
      if !strlen = maxstr then
        failwith "string too long in Afterstep look file";
      string lexbuf }

and end_of_line = parse
  |  '\n' | eof  { () }
  | [ ^ '\n']+ { end_of_line lexbuf }

and args = parse
    ' ' | '\t' { args lexbuf }
  | '"' { 
      strlen := 0; let s = string lexbuf in 
      fun n -> if n = 1 then [s] else s :: (args lexbuf (n-1)) }
  | [ ^ '"' ' ' '\t' '\n']+ {
      let s = Lexing.lexeme lexbuf in 
      fun n -> if n = 1 then [s] else s :: (args lexbuf (n-1)) }
  | '\n' { fun n ->
        let t = Array.create n "" in
        Array.to_list t
    }
  | eof { failwith "eof while waiting for args" }

{
type look = 
  Style of string * (look list)
| Font of string
| ForeColor of string
| BackColor of string
| MaxColors of int
| Inherit of string
| BackPixmap of int * string
| TextStyle of int
| ButtonPixmap of string
| MArrowPixmap of string
| MenuPinOn of string
| WindowFont of string
| IconFont of string
| TitleButton of int * string * string

| DefaultStyle of string
| FWindowStyle of string
| UWindowStyle of string 
| SWindowStyle of string 
| MenuItemStyle of string 
| MenuTitleStyle of string 
| MenuHiliteStyle of string 
| MenuStippleStyle of string 
  
let rec parse_all list = parser
    [< 'STYLE [style]; m = parse_all []; 'ENDSTYLE; s >] ->
    parse_all ((Style (style, m))::list) s
|   [< 'FONT [font]; s >] -> 
    parse_all ((Font font)::list) s
|   [< 'TEXTSTYLE [style]; s >] -> 
    parse_all ((TextStyle (int_of_string style))::list) s
|   [< 'FORECOLOR [color]; s >] -> parse_all ((ForeColor color)::list) s
|   [< 'BACKCOLOR [color]; s >] -> parse_all ((BackColor color)::list) s
|   [< 'MAXCOLORS [max]; s >] -> 
    parse_all ((MaxColors (int_of_string max))::list) s
|   [< 'INHERIT [style]; s >] -> parse_all ((Inherit style)::list) s
|   [< 'BACKPIXMAP [max;pixmap]; s >] -> 
    parse_all ((BackPixmap (int_of_string max,pixmap))::list) s
|   [< 'TEXTSTYLE [style]; s >] -> 
    parse_all ((TextStyle (int_of_string style))::list) s
|   [< 'BUTTONPIXMAP [pixmap]; s >] -> 
    parse_all ((ButtonPixmap pixmap)::list) s
|   [< 'MARROWPIXMAP [pixmap]; s >] -> 
    parse_all ((MArrowPixmap pixmap)::list) s
|   [< 'MENUPINON [pixmap]; s >] -> parse_all ((MenuPinOn pixmap)::list) s
|   [< 'WINDOWFONT [font]; s >] -> parse_all ((WindowFont font)::list) s
|   [< 'ICONFONT [font]; s >] -> parse_all ((IconFont font)::list) s
|   [< 'TITLEBUTTON [num;pix1;pix2]; s >] -> 
    parse_all ((TitleButton (int_of_string num, pix1,pix2))::list) s

|   [< 'DEFAULTSTYLE [style]; s >] -> parse_all ((DefaultStyle style)::list) s
|   [< 'FWINDOWSTYLE [style]; s >] -> parse_all ((FWindowStyle style)::list) s
|   [< 'UWINDOWSTYLE [style]; s >] -> parse_all ((UWindowStyle style)::list) s
|   [< 'SWINDOWSTYLE [style]; s >] -> parse_all ((SWindowStyle style)::list) s
|   [< 'MENUITEMSTYLE [style]; s >] -> parse_all ((MenuItemStyle style)::list) s
|   [< 'MENUTITLESTYLE [style]; s >] -> parse_all ((MenuTitleStyle style)::list) s
|   [< 'MENUHILITESTYLE [style]; s >] -> parse_all ((MenuHiliteStyle style)::list) s
|   [< 'MENUSTIPPLESTYLE [style]; s >] -> parse_all ((MenuStippleStyle style)::list) s
  
|   [< >] -> List.rev list
  
    
let parse_look lexbuf =
  let stream = Stream.from (fun n -> 
        match look lexbuf with
          EOF -> None
        | t ->   Some t) in
  parse_all [] stream
  
let open_look name =
  let dirname = 
    Utils.find_in_path !!Themes.asthemes_path name
  in
  let filename = Filename.concat dirname ("look." ^ (Filename.basename name))
  in
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let m = try parse_look lexbuf with e -> 
        Printf.printf "At pos %d:" (Lexing.lexeme_start lexbuf);
        print_newline ();
        close_in ic;
        raise e in
  close_in ic;
  m

let install_theme name =
  let look = open_look name in
  let dirname = 
    Utils.find_in_path !!Themes.asthemes_path name
  in
  root_image =:= ScaleImage (Filename.concat dirname
  ("background." ^ (Filename.basename name)));
  let styles = Hashtbl.create 31 in
  let rec getstyle list =
    match list with
      [] -> []
    | option :: list ->
        match option with
          Inherit style ->
            getstyle ((Hashtbl.find styles style) @ list)
        | _ -> option :: (getstyle list)
  in
  List.iter (fun option ->
      match option with
        Style (name, list) -> Hashtbl.add styles name list
          
      | FWindowStyle style ->
          active_font =:= !!default_font;
          active_foreground =:= !!default_foreground;
          active_background =:= !!default_background;
          active_image =:= NoImage;
          List.iter (fun option ->
              match option with
                Font font -> active_font =:= font
              | ForeColor color -> active_foreground =:= color
              | BackPixmap (_,filename) -> 
                  let filename =
                    if Filename.is_relative filename then
                      Filename.concat dirname filename
                    else filename 
                  in
                  active_image =:= TileImage filename
              | BackColor color -> active_background =:= color
              | _ -> ()
          ) (getstyle [Inherit style])
          
      | UWindowStyle style ->
          title_font =:= !!default_font;
          title_foreground =:= !!default_foreground;
          title_background =:= !!default_background;
          title_image =:= NoImage;
          List.iter (fun option ->
              match option with
                Font font -> title_font =:= font
              | ForeColor color -> title_foreground =:= color
              | BackPixmap (_,filename) -> 
                  let filename =
                    if Filename.is_relative filename then
                      Filename.concat dirname filename
                    else filename 
                  in
                  title_image =:= TileImage filename
              | BackColor color -> title_background =:= color
              | _ -> ()
          ) (getstyle [Inherit style])
          
      | MenuItemStyle style ->
          menu_font =:= !!default_font;
          menu_foreground =:= !!default_foreground;
          menu_background =:= !!default_background;
          menu_image =:= NoImage;
          List.iter (fun option ->
              match option with
                Font font -> 
                  menu_font =:= font;
                  iconMgr_font =:= font;
              | ForeColor color -> 
                  menu_foreground =:= color;
                  iconMgr_foreground =:= color
              | BackPixmap (_,filename) -> 
                  let filename =
                    if Filename.is_relative filename then
                      Filename.concat dirname filename
                    else filename 
                  in
                  menu_image =:= TileImage filename;
                  iconMgr_image =:= TileImage filename
              | BackColor color -> 
                  menu_background =:= color;
                  iconMgr_background =:= color
              | _ -> ()
          ) (getstyle [Inherit style])
          
      | MenuHiliteStyle style -> 
          menu_hilite_font =:= !!default_font;
          menu_hilite_foreground =:= !!default_background;
          menu_hilite_background =:= !!default_foreground;
          menu_hilite_image =:= NoImage;
          List.iter (fun option ->
              match option with
                Font font -> 
                  menu_hilite_font =:= font;
                  iconMgr_active_font =:= font;
              | ForeColor color -> 
                  menu_hilite_foreground =:= color;
                  iconMgr_active_foreground =:= color
              | BackPixmap (_,filename) -> 
                  let filename =
                    if Filename.is_relative filename then
                      Filename.concat dirname filename
                    else filename 
                  in
                  menu_hilite_image =:= TileImage filename;
                  iconMgr_active_image =:= TileImage filename
              | BackColor color -> 
                  menu_hilite_background =:= color;
                  iconMgr_active_background =:= color
              | _ -> ()
          ) (getstyle [Inherit style])
          
      | MenuTitleStyle style ->
          menu_title_font =:= !!default_font;
          menu_title_foreground =:= !!default_foreground;
          menu_title_background =:= !!default_background;
          menu_title_image =:= NoImage;
          List.iter (fun option ->
              match option with
                Font font -> 
                  menu_title_font =:= font;
                  iconMgr_title_font =:= font;
              | ForeColor color -> 
                  menu_title_foreground =:= color;
                  iconMgr_title_foreground =:= color;
              | BackPixmap (_,filename) -> 
                  let filename =
                    if Filename.is_relative filename then
                      Filename.concat dirname filename
                    else filename 
                  in
                  menu_title_image =:= TileImage filename;
                  iconMgr_title_image =:= TileImage filename;
              | BackColor color -> 
                  menu_title_background =:= color;
                  iconMgr_title_background =:= color
              | _ -> ()
          ) (getstyle [Inherit style])
          
      | _ -> ()) look; 
  Array.iter (fun s ->
      let sw = s#wob in
      (* reset menus *)
      (!menu_manager)#reset sw;      
      (* reset the clients *)
      List.iter (fun (c,w) ->
          let tw = w.w_top in
          let focus = Wob.getenv tw focus_var in
          update_focus tw.w_oo focus
      ) (list_clients sw)) !screens

let init () = ()
  
let available_themes () = 
  List.flatten (List.map (fun dir -> Utils.list_dir_normal dir) !!asthemes_path)

  
let menu _ =
  List.map (fun name ->
      name, [], Function (fun w -> install_theme name)) (available_themes ())
  
  
}