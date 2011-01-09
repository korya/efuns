(***********************************************************************)
(*                                                                     *)
(*                           Gwml                                      *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Xtypes
open Gwml_args

  
      
type image = 
  ScaleImage of string
| TileImage of string
| NoImage
  
type justified = Left | Center | Right

type binding = 
  Key of keySym * modifiers
| Button of button * modifiers
| DblClick of button * modifiers
| BMove of button * modifiers
  
type click = Simple | Double | DeltaMove | Other
  
type bindings = (binding * (wob -> unit) * bool) list
  
and wob_event =
| WobInit
| WobGetSize
| WobCreate
| WobResize of bool (* true=force; false=indication *)
| WobMove
| WobKeyPress of Xtypes.Xkey.t * string * Xtypes.keycode
| WobButtonPress of Xbutton.t
(* | WobDoubleClick of Xbutton.t *)
| WobUnmap of bool
| WobMap
| WobEnter
| WobLeave of bool (* true=leave window, false=enter child *)
| WobDestroy
| WobMapRequest
| WobRefresh
| WobIconifyRequest of bool (* true=from user; false=from program *)
| WobDeiconifyRequest of bool
| WobWithdrawn
| WobButtonRelease of Xbutton.t
| WobPropertyChange of atom
| WobSetInputFocus of bool
| WobDeleteWindow
| WobInstallColormap of bool (* true=reinstall; false=install *)
| WobMessage of string
| WobExitGwml
| WobRaiseWindow
| WobLowerWindow
| WobClientFocus of bool
| WobUpdateShape
| WobClickInClient
  
and wob = {
    mutable w_window : window;
    w_screen : screen_desc;
    w_geometry : geometry;
    mutable w_env : Wobenv.t;
    w_oo : wob_desc;
    mutable w_queue : wob_event list;
    w_parent : wob;
    w_top : wob;
  }

and cursor_desc =
  FontCursor of int
| BitmapCursor of string * string
| NoCursor

and bg_desc =
  BgColor of string
| BgPixmap of Xtypes.pixmap
  
and screen_desc = {
    s_clients : (client_desc * wob) Wintbl.t; 
       (* top window -> client desc *)
    s_scr : Xtypes.screen;
    s_colors : (string, pixel) Hashtbl.t;
    s_pixmaps : (string, Xpm.pixmap) Hashtbl.t;
    s_fonts : (string, font * queryFontRep) Hashtbl.t;
    s_cursors : (cursor_desc, cursor) Hashtbl.t;
    s_scheduler : Eloop.display;
    
    mutable s_last_cmap : colormap;    
    mutable s_cmap_wob : wob;
    mutable s_focus_wob : wob;
    mutable s_top_opening_hooks: (wob -> unit) list;
  }

and client_desc =
  { 
    c_window : window;
    c_screen : screen_desc;
    c_set_focus : bool; 
    mutable c_colormap : colormap;
    mutable c_new_window : bool;
    c_geometry : geometry;    
    mutable c_name : string;
    mutable c_icon_name : string;
    c_machine : string;
    c_class : string * string;
    mutable c_size_hints : wm_size_hints;
    mutable c_wm_hints : wm_hints;
    c_transient_for : window;
    mutable c_colormap_windows : window list;
    c_icon_size : wm_icon_size list;
    mutable c_wm_state : wmState;
    mutable c_wm_icon : window;
    mutable c_delete_window: bool;
    mutable c_take_focus: bool;
    mutable c_decorated: bool;
    mutable c_protocols: atom list;
    c_own_window : bool;
    mutable c_shaped : Shape.shapeQueryExtentsRep option;
  }

and wob_desc =
  <
  first_hook : (wob_event -> unit);
  last_hook : (wob_event -> unit);
  
  wob_hooks : hook list;
  actions : bindings;
  
  background : string;
  backimage : image;
  foreground : string;
  
  hilite_background : string;
  hilite_backimage : image;
  hilite_foreground : string;  
(*
  active_background : bg_desc;
  inactive_background : bg_desc;
  *)
  borderpixel : string;
  borderwidth : int;
  min_width : int;
  min_height : int;
  mask : eventMask list;
  refresh : unit;
  font : string;  
  
  add_hook : (hook -> unit);
  set_actions : (bindings -> unit);
(*
  set_active_background : (bg_desc -> unit);
  set_inactive_background : (bg_desc -> unit);
  *)
  set_background : (string -> unit);
  set_backimage : (image -> unit);
  set_foreground : (string -> unit);
  
  set_hilite_backimage : (image -> unit);
  set_hilite_background : (string -> unit);
  set_hilite_foreground : (string -> unit);
  
  set_backpixmap : (Xtypes.pixmap -> unit);
  set_borderpixel : (string -> unit);
  set_borderwidth : (int -> unit);
  set_cursor : (cursor_desc -> unit);
  set_min_width : (int -> unit);
  set_min_height : (int -> unit);
  set_mask : (eventMask list -> unit);
  set_font : (string -> unit);
  set_extensible_width : (int -> unit);  
  set_extensible_height : (int -> unit);  
  hilite : unit;
  unhilite : unit;
  hilitep : bool;
  set_hilite : (bool -> unit);
  reverse : bool;
  
  update_fg : unit;
  update_bg : unit;
  
  bg : string;
  fg : string;
  bgimage : image;
  
  handle_button : (Xtypes.Xbutton.t -> unit);
  handle_key : (Xtypes.Xkey.t * string * Xtypes.keySym -> unit);  
  create : bool -> unit;
  set_wob : (wob -> unit);
  wob : wob;
  xevents : (xevent -> unit);
  send : (wob_event -> unit);
  iter : (wob_desc -> unit) -> unit;


  resized : unit;
  client : client_desc;
  
  set_tip_display : (wob_desc -> bool -> unit) -> unit;
  
  is_shaped : bool;
  set_shaped : bool -> unit;
  >

and hook = (wob_event -> unit)
    
  
type pixmap_desc =
  FromFile of string
| FromFunction of string * (wob -> pixmap * pixmap)
| FromData of string * Xpm.pixmap_data
  
type bar_desc = Horizontal | Vertical
  
  (*************************************
  
          Initialisation  
  
  **************************************)

open Options
  
let graphics_path = define_option ["graphics_path"] 
  "<graphics_path> is the path where to look for graphics objects
  such as icons, pixmaps, images, etc ..." 
    path_option []
  
let _ =
  if !!graphics_path = [] then
    graphics_path =:= 
      [ 
      Utils.homedir;
      (Filename.concat Utils.homedir (".gwml-" ^ Version.gwml_version));
      (Filename.concat Utils.homedir ".pixmaps");
      (Sys.getcwd ());
      ("/usr/local/share/GwML/mini-icons");
      Version.gwml_lib;
      Version.installdir;
      ]
    
let add_to_path path dir =
  if not (List.mem dir !!path) then path =:= dir :: !!path
  
let screens = ref ([||] : wob_desc array)

let logp = define_option ["logp"] 
  "<logp> is true if you want GwML to log some debugging information
  in a file called <gwml.log>." bool_option false
let _ =
  Log.logp := !!logp;
  option_hook logp (fun _ ->
      Log.logp := !!logp)
  
let display = Xlib.openDisplay !dpyname

let use_shape = try
    Shape.queryExtension display
  with _ -> false
      
let use_imlib = define_option ["use_imlib"] 
  "<use_imlib> is false if you don't want to use the Imlib library.
  The Imlib library is only available on some systems (Linux + Gnome)." 
  bool_option true
  
let _ = 
  try Imager.image_init !dpyname with _ -> use_imlib =:= false
  
let (screen_opening_hooks : (wob -> unit) list ref) = ref []
let decorate_screen = ref (fun (wob : wob) -> ())
let decorate_client = ref (fun (sw : wob) (c : wob_desc) -> 
      (failwith "decorate_client not defined !" : unit))
  
  (*let _ = Xdebug.debug_flag := true*)
  (*let _ = Conv_event.debug := true*)

let debug = define_option ["debug"] 
  "<debug> is a flag that you can use (with <logp>) to make GwML
  print some debugging information." bool_option false
let debug_events = define_option ["debug_events"] 
  "<debug_events> is true if you GwML to log all events received by
  wobs." bool_option false
  
let xdefaults = try Sys.getenv "XUSERFILESEARCHPATH" with
    Not_found -> Filename.concat Utils.homedir ".Xdefaults"

let x_res = Xrm.create ()
let _ =
  begin    
    try
      let gwml_res = 
        let path = try Utils.string_to_path (Sys.getenv "XFILESEARCHPATH") with _ -> 
              [] in
        let xenv = try Sys.getenv "XENVIRONMENT" with _ -> "" in
        let xroot = try Filename.concat  (Sys.getenv "X11ROOT")
            "lib/X11/app-defaults/" with _ -> "" in
        Utils.find_in_path (path@[
            xenv; xroot; "/usr/X11/lib/X11/app-defaults/"]) "Gwml"
      in
      Xrm.safe_load x_res gwml_res
    with _ -> ()
  end;
  Xrm.safe_load x_res xdefaults

let fixed_font = 
  let font = X.openFont display "fixed" in
  let qf_font = X.queryFont display font in
  (font,qf_font)

let font_make w font_name =
  let font_name = String.lowercase font_name in
  let s = w.w_screen in
  try
    Hashtbl.find s.s_fonts font_name
  with
    Not_found ->
      try
        let font = X.openFont display font_name in
        let qf = X.queryFont display font in
        let font = (font,qf) in
        Hashtbl.add s.s_fonts font_name font;
        font
      with
        _ -> fixed_font

let hex c =
  let c = Char.lowercase c in
  if c >='0' && c<='9' then Char.code c - Char.code '0' else
  if c >= 'a' && c<='f' then 10 + Char.code c - Char.code 'a' else
    raise Not_found

type color_spec =
  NamedColor of string
| RgbColor of int * int * int
    
let parse_color c = 
  if String.length c = 7 && c.[0] = '#' then
    let r = hex c.[2] + 16 * hex c.[1] in let r = 256 * r in
    let g = hex c.[4] + 16 * hex c.[3] in let g = 256 * g in
    let b = hex c.[6] + 16 * hex c.[5] in let b = 256 * b in 
    RgbColor (r,g,b)
  else
  if String.length c = 13 && c.[0] = '#' then
    let r = hex c.[2] + 16 * hex c.[1] in
    let g = hex c.[6] + 16 * hex c.[5] in
    let b = hex c.[10] + 16 * hex c.[9] in
    RgbColor (r,g,b)
  else
  if String.length c = 12 && 
    String.sub c 0 4 = "rgb:" && c.[6] = '/'  && c.[9] = '/' then
    let r = hex c.[5] + 16 * hex c.[4] in let r = 256 * r in
    let g = hex c.[8] + 16 * hex c.[7] in let g = 256 * g in
    let b = hex c.[11] + 16 * hex c.[10] in let b = 256 * b in 
    RgbColor (r,g,b)
  else
    NamedColor c

  
let color_make w color =
  let color = String.lowercase color in  
  let s = w.w_screen in
  try
    Hashtbl.find s.s_colors color
  with
    Not_found ->
      let cmap = s.s_scr.scr_default_colormap in
      try
        let pixel =
          match parse_color color with
            RgbColor (r,g,b) ->
              (X.allocColor display cmap r g b).ac_pixel
          | NamedColor color ->
              (X.allocNamedColor display cmap color)
              .anc_pixel
        in
        Hashtbl.add s.s_colors color pixel;
        pixel
      with
        _ -> s.s_scr.scr_black_pixel

          (* This has to be improved to be able to load pixmaps from images
          using the Imlib stuff. Another approach would b e to use Imlib
          for all pixmaps including .xpm ones, and generated ones (can be
        generated to a file ... *)
          

let exec_hooks arg hooks =
  List.iter (fun hook -> try hook arg with _ -> ()) hooks
          
let ungrab_hooks = ref []
  
let grabServer () =
  if not !server_grabbed then begin
      X.grabServer display;
      server_grabbed := true
    end else
    failwith "Already grabbed"
    
let ungrabServer () =
  if !server_grabbed then begin
      X.ungrabServer display;
      server_grabbed := false;
      let hooks = !ungrab_hooks in
      ungrab_hooks := [];
      exec_hooks () hooks;
    end

let ungrab_exec hook =
  if !server_grabbed then
    ungrab_hooks := hook :: !ungrab_hooks
  else
    hook ()
          
let pixmap_make w pixmap =
  let s = w.w_screen in
  let name =
    match pixmap with
      FromFile file -> file
    | FromFunction (name,_) -> name
    | FromData (name,_) -> name
  in
  try Hashtbl.find s.s_pixmaps name with Not_found ->
      let scr = s.s_scr in
      let pixmap = 
        match pixmap with
          FromFile file ->
            let file = Utils.find_in_path !!graphics_path file in
            (try
                Xpm.createPixmapFromFile display scr.scr_root
                  scr.scr_default_colormap scr.scr_root_depth file
              with _ when !!use_imlib && not !server_grabbed ->
              (* Maybe we can use Imlib to render this image ... *)
                  let im = Imager.image_load file in
                  let pix = Imager.image_pixmap im im.Imager.w im.Imager.h in
                  Imager.image_destroy im;
                  im.Imager.w, im.Imager.h, 2, pix.Imager.pixmap, pix.Imager.mask 
            )
        | FromFunction (name,f) -> 
            let (pix,mask) = f w in
            let gg = X.getGeometry display pix in
            gg.gg_width, gg.gg_height, gg.gg_depth, pix, mask
        | FromData (name,data) ->
            Xpm.createPixmapFromData display scr.scr_root
              scr.scr_default_colormap scr.scr_root_depth data
      in
      Hashtbl.add s.s_pixmaps name pixmap;
      pixmap

let cursor_make w name =
  let s = w.w_screen in
  try
    Hashtbl.find s.s_cursors name
  with
    Not_found ->
      let screen = s.s_scr in
      let cursor = 
        match name with
          FontCursor name ->
            Xlib.createFontCursor display name 
        | NoCursor -> noCursor
        | BitmapCursor (bitmap,mask) ->
            let bitmap = Utils.find_in_path !!graphics_path bitmap in
            let mask = Utils.find_in_path !!graphics_path mask in
            let (w,h,hot_x,hot_y,pix) = 
              Xpm.createBitmapFromFile display screen.scr_root bitmap in
            try
              let (ww,hh,_,_,mask) = 
                Xpm.createBitmapFromFile display screen.scr_root mask in
              try
                if w = ww &&  h = hh then
                  X.createCursor display pix mask 0 0 0 0xFFFF 0xFFFF 0xFFFF
                    hot_x hot_y else raise Not_found 
              with e -> X.destroyWindow display mask; raise e
            with _ -> X.destroyWindow display pix;
                Xlib.createFontCursor display XC.xc_x_cursor 
      in
      Hashtbl.add s.s_cursors name cursor;
      cursor
        
  


let xa_wm_protocols = X.internAtom display "WM_PROTOCOLS" true
let xa_wm_colormap_windows = X.internAtom display "WM_COLORMAP_WINDOWS" true
let xa_wm_state = X.internAtom display  "WM_STATE" true
let xa_wm_take_focus = X.internAtom display "WM_TAKE_FOCUS" true
let xa_wm_delete_window = X.internAtom display "WM_DELETE_WINDOW" true
let xa_wm_change_state = X.internAtom display "WM_CHANGE_STATE" true
  
let print_event e =
  match e with
  | WobInit -> "WobInit"
  | WobGetSize -> "WobGetSize"
  | WobCreate -> "WobCreate"
  | WobResize _ -> "WobResize"
  | WobMove -> "WobMove"
  | WobKeyPress _  -> "WobKeyPress"
  | WobButtonPress _  -> "WobButtonPress"
(*  | WobDoubleClick _  -> "WobDoubleClick" *)
  | WobUnmap _ -> "WobUnmap"
  | WobMap -> "WobMap"
  | WobEnter -> "WobEnter"
  | WobLeave _ -> "WobLeave"
  | WobDestroy -> "WobDestroy"
  | WobMapRequest -> "WobMapRequest"
  | WobRefresh -> "WobRefresh"
  | WobIconifyRequest _ -> "WobIconifyRequest"
  | WobDeiconifyRequest _ -> "WobDeiconifyRequest"
  | WobWithdrawn -> "WobWithdrawn"
  | WobButtonRelease _ -> "WobButtonRelease"
  | WobPropertyChange _ -> "WobPropertyChange"
  | WobSetInputFocus b -> Printf.sprintf "WobSetInputFocus %s" (string_of_bool b)
  | WobDeleteWindow -> "WobDeleteWindow"
  | WobInstallColormap _ -> "WobInstallColormap"
  | WobMessage _ -> "WobMessage"
  | WobExitGwml -> "WobExitGwml"
  | WobRaiseWindow -> "WobRaiseWindow"
  | WobLowerWindow -> "WobLowerWindow"
  | WobClientFocus b -> Printf.sprintf "WobClientFocus %s" (string_of_bool b)
  | WobUpdateShape -> "WobUpdateShape"
  | WobClickInClient -> "WobClickInClient"
      
let top_cursor = Xlib.createFontCursor display XC.xc_top_side  
let bottom_cursor = Xlib.createFontCursor display XC.xc_bottom_side  
let left_cursor = Xlib.createFontCursor display XC.xc_left_side  
let right_cursor = Xlib.createFontCursor display XC.xc_right_side  

  
let rec root_position w =
  if w.w_parent == w then 0,0 else
  let x,y = root_position w.w_parent in
  let g = w.w_geometry in
  x+g.x+g.border, y+g.y+g.border
  
  (*************************************************************
  
  THE NEW INTERFACE
  
  *************************************************************)
  
  (* The list of targets for WM-events *)
  
type wm_event = client_desc * wob * client_event

and client_event =
| AddClient
| RemoveClient
| ClientResize
| ClientMove
| ClientUnmap
| ClientMap
| ClientIconify
| ClientDeiconify
| ClientPropertyChange of atom
| ClientColormap of bool
| ClientFocus of bool
  
let broadcast_targets = ref []
  
let wm_broadcast (ev : wm_event) =
  List.iter (fun target -> try target ev with _ -> ()) !broadcast_targets
  
let _ =
  class_hook font_option (fun option ->
      try
        let _ = Xsync.openFont display !!option in
        ()
      with _ ->
          option =:= "fixed"
  )
  
let prevent_animation = ref true
let final_actions = ref ([] : (unit -> unit) list)

let config_loaded = ref false
  
  
let is_mapped = Wobenv.new_var ()


let delay time = ignore (Concur.ThreadUnix.select [] [] [] time)
  