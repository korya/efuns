(***********************************************************************)
(*                                                                     *)
(*                             ____________                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Xtypes
open Xlib
open Gwml
open Options

(******** TYPES *******)
  
type item_option = 
  ItemPixmap of (pixmap_desc * bool)  
| ItemForeground of (unit -> string)
| ItemBackground of (unit -> string)
| ItemFont of (unit -> string)
| ItemHiliteForeground of (unit -> string)
| ItemHiliteBackground of (unit -> string)
| ItemHiliteFont of (unit -> string)
| ItemTitle
  
type menu = (string * item_option list * action) list (* "label", custom, action *)
  
and action =
  NoAction
| Menu of (unit -> menu)
| ActiveMenu of (wob -> menu)
| Function of func
| NamedAction of string
| NamedActionX of string * string list
  
and func = wob -> unit

let (actions : (string, action) Hashtbl.t) = Hashtbl.create 63
let (xactions : (string, string list -> action) Hashtbl.t) = Hashtbl.create 63

let define_action action_name action_fun =
  Hashtbl.add actions action_name action_fun
let define_xaction action_name action_fun =
  Hashtbl.add xactions action_name action_fun
  
let find_action action_name =
  try
    Hashtbl.find actions action_name
  with _ -> 
      Printf.printf "No action %s" action_name; print_newline ();
      NoAction

let find_xaction action_name args =
  try
    (Hashtbl.find xactions action_name) args
  with _ -> NoAction
      
let rec get_action action =
  match action with
    NamedAction action -> get_action (find_action action)
  | NamedActionX (action,args) -> get_action (find_xaction action args)
  | _ -> action

let _ =
  define_action "no_action" NoAction
  
(********** .gwmlrc OPTIONS ***********)

      
let smart_install = define_option ["smart_install"] 
    "Flag enabling search of directories (pixmaps, themes, etc)"
    bool_option true
  
let _ =
  if !Gwml_args.install then smart_install =:= true;
  final_actions := (fun _ -> smart_install =:= false) :: !final_actions

  
let value_to_image v = match v with
    Options.Value "" -> NoImage
  | Options.Value s -> ScaleImage (Utils.string_to_filename s)
  | Options.List [Options.Value "spixmap"; Options.Value s] ->
      ScaleImage (Utils.string_to_filename s)
  | Options.List [Options.Value "tpixmap"; Options.Value s] ->
      TileImage (Utils.string_to_filename s)
  | _ -> raise Not_found

let image_to_value v = 
  match v with
    NoImage -> Options.Value ""
  | ScaleImage s -> Options.SmallList 
      [Options.Value "spixmap"; Options.Value (Utils.filename_to_string s)]
  | TileImage s -> Options.SmallList 
      [Options.Value "tpixmap"; Options.Value (Utils.filename_to_string s)]

let image_option = define_option_class "Image" value_to_image image_to_value
      
let root_image = define_option ["root_image"] 
  "The image used in the background of the screen. The format of the option
  is : (spixmap, image_file) or (tpixmap, image_file) where
    spixmap means scaled image, and tpixmap means tiled image." 
    image_option NoImage
  
let root_background = define_option ["root_background"] 
  "<root_background> is the color the should be displayed in the background
  of the screen if no image is specified." string_option "cadetblue"

  
let active_background = 
  define_option ["active_background"] 
  "<active_background> is the background color of the title bar of
a window owning the focus."
    color_option "blue"
  
let active_foreground = 
  define_option ["active_foreground" ] 
  "<active_foreground> is the foreground color of the label in the title
bar of the window owning the focus"
    color_option "white"
  
let active_font = define_option ["active_font"] "" font_option "fixed"
let active_image = define_option ["active_image"] "" image_option NoImage


  
    (*********   boolean options  **********)  
let editable_title = 
  define_option ["editable_title"] 
  "<editable_title> is true if the window title can be edited to change its title."
  bool_option false
  
let do_animation = 
  define_option ["do_animation"] 
  "<do_animation> indicates if animations should be drawn during iconification, etc ...."
    bool_option true

let pan_on_click = 
  define_option ["pan_on_click"] 
  "<pan_on_click> indicates whether the user should click on the screen
  borders to move between virtual screens."
  bool_option false

let confine_move = define_option ["confine_move"] 
  "<confine_move> is true if you want to prevent the user from moving 
  client windows half-outside the screen " bool_option true
let opaque_move = define_option ["opaque_move"] 
    "<opaque_move> is true if you want to see the contains of a window
    while moving it" bool_option  false
  
let grab_server = define_option ["grab_server"] "" bool_option true
let auto_raise = define_option ["auto_raise"] 
  "<auto_raise> is true if you want to raise the window owning the focus.
  This can be further configured by setting the <auto_raise_delay>" 
  bool_option true
let auto_colormap = define_option ["auto_colormap"] 
  "<auto_colormap> is true if you want to automatically activate the 
  colormap of the window owning the focus." bool_option true
let group_iconification = define_option ["group_iconification"] 
  "<group_iconification> is true if you want to iconify groups of
    windows (all transient windows are iconified with the main window." 
  bool_option true
let windows_corner = define_option ["windows_corner"] "" bool_option false
let debug_command = define_option ["debug_command"] "" bool_option false
  
let resize_font = define_option ["resize_font"] "" font_option "fixed"
let window_font = define_option ["window_font"] 
    "<window_font> is the font used in the title of a client window." 
  font_option 
  "-adobe-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*"

let window_background = 
  define_option ["window_background"] "" color_option "black"
let window_foreground = 
  define_option ["window_foreground"] "" color_option "white"

let title_background = 
  define_option ["title_background"] 
  "<title_background> is the background color used in the title
  of a client window." color_option "black"
let title_image =
  define_option ["title_image"] 
  "<title_image> is the image displayed in the background of the title
  of a client window" image_option NoImage
let title_foreground = 
  define_option ["title_foreground"] "" color_option "white"
let title_font = define_option ["title_font"] "" font_option "fixed"
  
let sticky_foreground = 
  define_option ["sticky_foreground"] "" color_option "black"
let sticky_background = 
  define_option ["sticky_background"] "" color_option "white"

let justification_option = sum_option 
    ["left", Left; "center", Center; "right", Right]
let title_justification = define_option ["title_justification"] ""
    justification_option Left
  
let edge_scrolling_vertic = 
  define_option ["edge_scrolling_vertic"] 
  "<edge_scrolling_vertic> is the percent of screen height scrolled during
  vertical virtual move" int_option 100
let edge_scrolling_horiz = 
  define_option ["edge_scrolling_horiz"] 
  "<edge_scrolling_horiz> is the percent of screen width scrolled during
  horizontal virtual move" int_option 100
let edge_scrolling_resist = 
  define_option ["edge_scrolling_resist"] "" int_option 10
let edge_moving_resist = 
  define_option ["edge_moving_resist"] 
  "<edge_moving_resist> is the time (in ms) that the cursor must spend on
    the screen border to trigger a virtual move (with <pan_on_click> false)." 
  float_option 10.
let opaque_move_size = 
  define_option ["opaque_move_size"] 
  "<opaque_move_size> is the size in pixel of the maximal window area" int_option 100

let iconMgr_title_foreground = 
  define_option ["iconMgr_title_foreground"] "" color_option "white"
let iconMgr_title_background = 
  define_option ["iconMgr_title_background"] "" color_option "black"
let iconMgr_title_image = 
  define_option ["iconMgr_title_image"] "" image_option NoImage
let iconMgr_title_font = 
  define_option ["iconMgr_title_font"] "" font_option "fixed"

let iconMgr_active_foreground = 
  define_option ["iconMgr_active_foreground"] "" color_option "white"
let iconMgr_active_background = 
  define_option ["iconMgr_active_background"] "" color_option "black"
let iconMgr_active_image = 
  define_option ["iconMgr_active_image"] "" image_option NoImage
let iconMgr_active_font = 
  define_option ["iconMgr_active_font"] "" font_option "fixed"

let iconMgr_foreground = 
  define_option ["iconMgr_foreground"] "" color_option "black"
let iconMgr_background = 
  define_option ["iconMgr_background"] "" color_option "white"
let iconMgr_image = 
  define_option ["iconMgr_image"] "" image_option NoImage
let iconMgr_font = 
  define_option ["iconMgr_font"] "" font_option "fixed"
  
let icon_foreground = 
  define_option ["icon_foreground"] "" color_option  "white"
let icon_background = 
  define_option ["icon_background"] "" color_option "black"
let icon_font = 
  define_option ["icon_font"] "" font_option "fixed"
let icon_borderwidth = 
  define_option ["icon_borderwidth"] "" int_option 3

let frame_borderwidth = 
  define_option ["frame_borderwidth"] 
  "<frame_borderwidth> is the border width of the external frame of
  a client window (ie outside the decorations)." int_option 1
  
let menu_font = 
  define_option ["menu_font"] "" 
  font_option "-adobe-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*"  
let menu_foreground = 
  define_option ["menu_foreground"] "" color_option "white"
let menu_background = 
  define_option ["menu_background"] "" color_option "black"
let menu_image =
  define_option ["menu_image"] "" image_option NoImage
    
let menu_hilite_font = 
  define_option ["menu_hilite_font"] "" 
  font_option "-adobe-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*"  
let menu_hilite_foreground = 
  define_option ["menu_hilite_foreground"] "" color_option "black"
let menu_hilite_background = 
  define_option ["menu_hilite_background"] "" color_option "white"
let menu_hilite_image =
  define_option ["menu_hilite_image"] "" image_option NoImage
  
let menu_title_image =
  define_option ["menu_title_image"] "" image_option NoImage

let menu_title_font = 
  define_option ["menu_title_font"] "" 
    font_option "-adobe-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*"
  
let menu_title_foreground = 
  define_option ["menu_title_foreground"] "" color_option "black"
let menu_title_background = 
  define_option ["menu_title_background"] "" color_option "white"
  
let panner_foreground = 
  define_option ["panner_foreground"] "" color_option "black"
let panner_background  = 
  define_option ["panner_background"] "" color_option "white"

let auto_raise_delay = 
  define_option ["auto_raise_delay"] 
  "<auto_raise_delay> is the minimal period during which a window
    must own the focus before being raised (if auto_raise is true)." 
  float_option 0.05
  
let animation_delay =
  define_option ["animation_delay"] 
  "<animation_delay> is the small period between two drawing of the
  animation during an iconfication, move, etc..." float_option 0.0001
  
  (********* CONTEXT VARS *********)  

let warp_offset = define_option ["warp_offset"] 
  "<warp_offset> is the offset used after a virtual move to move the cursor far enough from the screen border to prevent unwanted repeat of virtual moves
  (if <pan_on_click> is false)." 
    int_option 20 
  
let prevent_autoraise = ref false

let (is_iconified_var : bool Wobenv.var) = Wobenv.new_var ()
let (is_in_dvroom_var : bool Wobenv.var) = Wobenv.new_var ()
let (is_group_iconified_var : bool Wobenv.var) = Wobenv.new_var ()
let (is_minimized_var : int Wobenv.var) = Wobenv.new_var ()
let (focus_var : bool Wobenv.var) = Wobenv.new_var ()
let (icon_var : (Top.top * Xtypes.window list ref) Wobenv.var) = Wobenv.new_var ()
let old_width = Wobenv.new_var () (* for maximize/minimize *)
let old_height = Wobenv.new_var ()

  (* This function should be used to know if a function is in the 
  current workspace and not iconified. *)
let is_viewable w =
  let w = w.w_top in
  (not (Wob.sgetenv w is_iconified_var false)) &&
  (Wob.sgetenv w is_in_dvroom_var true)
  
let command cmd =
  if !!debug_command then begin
      Printf.printf "Command: %s" cmd;
      print_newline ();
    end;
  Sys.command cmd
  
let comp_name = ref (fun (x : string) -> x)

let string_to_modifier s =  
  let mask = ref 0 in
  for i = 0 to String.length s - 1 do
    mask := !mask lor (match s.[i] with
        'S' -> shiftMask
      | 'C' -> controlMask
      | 'A' -> anyModifier
      | 'M' -> mod1Mask
      | '1' -> mod1Mask
      | '2' -> mod2Mask
      | '3' -> mod3Mask
      | '4' -> mod4Mask
      | '5' -> mod5Mask
      | _ -> 0
    )
  done;
  if !mask land anyModifier <> 0 then anyModifier else !mask
  
let modifier_to_string mask = 
  let s = if mask land shiftMask = 0 then "" else "S" in
  let s = if mask land controlMask = 0 then s else "C" ^ s in
  let s = if mask land mod1Mask = 0 then s else "M" ^ s in
  let s = if mask land mod2Mask = 0 then s else "2" ^ s in
  let s = if mask land mod3Mask = 0 then s else "3" ^ s in
  let s = if mask land mod4Mask = 0 then s else "4" ^ s in
  let s = if mask land mod5Mask = 0 then s else "5" ^ s in
  if mask = anyModifier then "A" else s
  
let modifiers_option = define_option_class "Modifiers" 
    (fun v -> match v with
        Value s -> string_to_modifier s
      | _ -> raise Not_found)
  (fun mask -> Value (modifier_to_string mask))

let wm_modifiers = 
  define_option ["wm_modifiers"] "" 
  modifiers_option (controlMask lor mod1Mask)

let _ =
  let s = !Gwml_args.wm_mods in
  if s <> "" then wm_modifiers =:= string_to_modifier s
    
  
let icon_path = define_option ["icon_path"] "" path_option
    ([]: string list)
  
  (* let title_font = ref "fixed"  *)
let last_iconify_windows = ref []
    
(*let message_label = ref
let message_x = ref 10
let message_y = ref 10
    *)

type pager = { 
    pager_window : window;
    pager_width : int;
    pager_height : int;
  }
  
  
class type virtual_manager = object 
  method pager : (wob -> pager)
  method update : (wob -> unit)
  method move : (wob -> int -> int -> unit)
  method start : (wob -> unit)
  method omit_move : (wob -> bool -> unit)
  method drawp : (wob -> bool)
  method movep : (wob -> bool)
  method omit_draw : (wob -> bool -> unit)
  method goto : (wob -> unit)
  method place_on_screen : (wob -> unit)
  method current_position : (wob -> int * int)
  method add_hook : (wob -> (wob -> unit) -> unit)
end

class no_virtual = (object 
  method pager sw = raise Not_found
  method update sw = ()
  method move sw dx dy = ()
  method start sw = ()
  method omit_draw w b = ()
  method omit_move w b = ()
  method drawp w = true
  method movep w = true
  method goto (w : wob) = ()
  method place_on_screen (w : wob) = ()
  method current_position _ = 0,0
  method add_hook _ _ = 
    Printf.printf "Setting hook in noVirtualManager";
    print_newline ();
end
  : virtual_manager)

let virtual_manager = ref (new no_virtual : virtual_manager)

let toggle_virtual_move w =
  let p = (!virtual_manager)#movep w in
  (!virtual_manager)#omit_move w p
      
class type icon_manager = object
  method hook : (client_desc -> wob_desc -> wob_event -> unit)
  method create : wob -> int -> int -> string -> int -> (client_desc -> bool) -> unit
  method message : wob -> string -> unit
end



class no_iconMgr = (object
  method hook c w e = ()
  method create w x y title force predicat = ()
  method message w m = ()
end : icon_manager)


class type desk_manager = object
  method goto : (wob -> unit)
  method hook : (client_desc -> wob_desc -> wob_event -> unit)
  method new_desk : wob -> string -> unit
  method to_desk : wob -> string -> unit
  method rename_desk : wob -> string -> string -> unit
  method add_to_desk : wob -> string -> unit
end

class no_desk_manager = (object
  method goto w = ()
  method hook c w e = ()
  method new_desk w s = ()
  method to_desk w s = ()
  method rename_desk w s1 s2 = ()
  method add_to_desk s w = ()
end : desk_manager)

let desk_manager = ref (new no_desk_manager)
  
let desk_manager_hook c (tw : Top.top) = (!desk_manager)#hook c (tw :> wob_desc)







let resize_mode = ref (fun (w : wob) (from_event : bool) -> ())
let resize w f = !resize_mode w f

let icon_manager = ref (new no_iconMgr : icon_manager)

let icon_manager_hook c (tw : Top.top) v=
  !icon_manager#hook c (tw :> wob_desc) v

type decoration = wob -> client_desc -> 
( (Top.top -> hook) list *
  wob_desc option * wob_desc option* wob_desc option * wob_desc option)

type placement = client_desc -> wob -> unit

module Parameters = struct
    type t = 

    (* GwML options *)
      Placement of placement
    | Decoration of decoration
    | Icon of string
      
    (* These are fvwm decorations *)
    | BorderWidth of int
    | HandleWidth of int
    | NoTitle
    | Sticky
    | WindowListSkip
    | StaysOnTop
    | ClickToFocus
    | NoHandles 
    | StartsOnDesk of int
    | TitleIcon of string
    | NoPPosition
    | SmartPlacement
    | RandomPlacement
    | StubbornPlacement
    | SloppyFocus
    | NoIcon
    | MWMDecor
    | MWMFunctions
    | HintOverride
    | DecorateTransient
    | CirculateSkip
    | ForeColor of string
    | BackColor of string
    | NoButton of int
    | Color of string * string
    | StartIconic
  end  

  (* We should use a better option table, based on < class * name * title > *)
  
let name_options = (Opttable.create () : Wobenv.env Opttable.t)
let class_options = (Opttable.create () : Wobenv.env Opttable.t)
let complete_options = (Opttable.create () : Wobenv.env Opttable.t)

let std_options_var = Wobenv.new_var ()
  
let add_std_options name options =
  let name = if name = "" then "*" else name in
  let name, table = 
    if name = "*" then name, class_options else
    let len = String.length name in
    let c = name.[0] in
    if c = '.' then String.sub name 1 (String.length name - 1), name_options 
    else
    let rec point n = 
      if n < len then if name.[n] = '.' then true else point (n+1) else false
    in
    if point 0 then name, complete_options else
    let c = name.[0] in
    if c >= 'a' && c <= 'z' then name, name_options else name, class_options
  in
  let option_base = try Opttable.find table name with 
      Not_found -> Wobenv.empty_env ()
  in
  let option_base = Wobenv.copy_env option_base in
  Opttable.add table name option_base;
  let old_options = try Wobenv.get_env option_base std_options_var with _ -> 
        [] in
  Wobenv.set_env option_base std_options_var (options@old_options)
  
let add_option name option_var option =
  let name = if name = "" then "*" else name in
  let name, table = 
    if name = "*" then name, class_options else
    let len = String.length name in
    let c = name.[0] in
    if c = '.' then String.sub name 1 (len - 1), name_options 
    else
    let rec point n = 
      if n < len then if name.[n] = '.' then true else point (n+1) else false
    in
    if point 0 then name, complete_options else
    let c = name.[0] in
    if c >= 'a' && c <= 'z' then name, name_options else name, class_options
  in
  let option_base = try Opttable.find table name with 
      Not_found -> Wobenv.empty_env ()
  in
  let option_base = Wobenv.copy_env option_base in
  Opttable.add table name option_base;
  Wobenv.set_env option_base option_var option
  
let add_placement placement (classe,name) =
  let name = 
    if name = "" then  classe else
    if classe = "" then name else
      classe^"."^name
  in
  add_std_options name [Parameters.Placement placement]

let add_icon icon (classe,name) =
  let name = 
    if name = "" then  classe else
    if classe = "" then name else
      classe^"."^name
  in
  add_std_options name [Parameters.Icon icon]
  
let add_decoration decoration (classe,name) =
  let name = 
    if name = "" then  classe else
      if classe = "" then name else
      classe^"."^name
  in
  add_std_options name [Parameters.Decoration decoration]
  
let find_std_options classe name =
  let options1 =  try 
      let option_base = Opttable.find complete_options (classe^"."^name) in
      Wobenv.get_env option_base std_options_var
    with
      Not_found ->  [] in
  let options2 =  try 
      let option_base = Opttable.find name_options name in
      Wobenv.get_env option_base std_options_var
    with
      Not_found -> [] in
  let options3 = try 
      let option_base = Opttable.find class_options classe in
      Wobenv.get_env option_base std_options_var
    with
      Not_found -> [] in
  let options4 = try     
      let option_base = Opttable.find class_options "" in
      Wobenv.get_env option_base std_options_var
    with
      Not_found -> [] 
  in
  options1@options2@options3@options4
  
let find_option c option_var =
  let (classe,name) = c.c_class in  
  let wm_name = c.c_name in
  try 
    let option_base = Opttable.find complete_options (classe^"."^name) in
    Wobenv.get_env option_base option_var
  with Not_found ->
      try 
        let option_base = Opttable.find name_options name in
        Wobenv.get_env option_base option_var
      with Not_found ->
          try 
            let option_base = Opttable.find name_options wm_name in
            Wobenv.get_env option_base option_var
          with Not_found ->
              try 
                let option_base = Opttable.find class_options classe in
                Wobenv.get_env option_base option_var
              with Not_found ->
                  let option_base = Opttable.find class_options "" in
                  Wobenv.get_env option_base option_var
                  
let find_placement c =
  let (classe,name) = c.c_class in
  let options = find_std_options classe name in
  let rec iter list =
    match list with
      [] -> raise Not_found
    | head :: tail -> 
        match head with
          Parameters.Placement placement -> placement
        | _ -> iter tail in
  iter options

let find_icon  c =
  let (classe,name) = c.c_class in
  let options = find_std_options classe name  in
  let rec iter list =
    match list with
      [] -> raise Not_found
    | head :: tail -> 
        match head with
          Parameters.Icon icon -> icon
        | _ -> iter tail in
  iter options
  
let find_decoration  c =
  let (classe,name) = c.c_class in
  let options = find_std_options classe name  in
  let rec iter list =
    match list with
      [] -> raise Not_found
    | head :: tail -> 
        match head with
          Parameters.Decoration decoration -> decoration
          | _ -> iter tail in
  iter options
  
let modulo x w =
  if x >= 0 then x mod w
  else
    (x mod w) + w
    
let div x w =
  if x >= 0 then x / w else (x / w) - 1

    
let goto_window w =
  let w = w.w_top in
  let s = w.w_screen.s_scr in
  (!desk_manager)#goto w;
  (!virtual_manager)#goto w;
  let c = w.w_oo#client in
  if c.c_wm_state = IconicState || c.c_wm_state = WithdrawnState then
    (try Wob.send w (WobDeiconifyRequest true) with _ -> ());
  Wob.send w.w_top WobRaiseWindow;
  X.warpPointer display s.scr_root 0 0 s.scr_width s.scr_height 
    w.w_window 20 20
  
let x_maximize_use = define_option ["x_maximize_use"] "" int_option 95
let y_maximize_use = define_option ["y_maximize_use"] "" int_option 95
  
let maximize_x w =
  let tw = w.w_top in
  let s = tw.w_screen in
  let scr = s.s_scr in
  let tg = tw.w_geometry in
  let (old_x, old_dx) = try Wob.getenv tw old_width with
      Not_found ->
        let dx = ref (scr.scr_width * !!x_maximize_use / 100) in
        let x = ref (
            if tg.x + !dx > scr.scr_width then 
              scr.scr_width * (100 - !!x_maximize_use) / 100 
            else tg.x)
        in
        Wob.setenv tw old_width (x,dx);
        (x,dx)        
  in
  let tmp_x = tg.x in
  tg.x <- !old_x;
  let tmp_width = !old_dx in
  old_x := tmp_x;
  old_dx := tg.width;
  Top.resize_top tw tmp_width tg.height

let maximize_y w =
  let tw = w.w_top in
  let s = w.w_screen in
  let scr = s.s_scr in
  let tg = tw.w_geometry in
  let (old_y,old_dy) = try Wob.getenv tw old_height with
      Not_found ->
        let dy = ref (scr.scr_height * !!y_maximize_use / 100) in
        let y = ref (
            if tg.y + !dy > scr.scr_height then 
              scr.scr_height * (100 - !!y_maximize_use) / 100 
            else tg.y)
        in
        Wob.setenv tw old_height (y,dy);
        (y,dy)
  in
  let tmp_y = tg.y in
  tg.y <- !old_y;
  let tmp_height = !old_dy in
  old_y := tmp_y;
  old_dy := tg.height;
  Top.resize_top tw tg.width tmp_height

let map_all sw =
  Wintbl.iter (fun win (c,w) ->
      let scr = w.w_screen.s_scr in
      let w = w.w_top in
      let g = w.w_geometry in
      g.x <- modulo g.x scr.scr_width;
      g.y <- modulo g.y scr.scr_height;
      Wob.send w WobMap;
  ) sw.w_screen.s_clients

let info c w =
  let w = w.w_top in
  let g = w.w_geometry in
  let s= 
    Printf.sprintf "%d: %s.%s : %dx%d+%d+%d" c.c_window (fst c.c_class) (snd c.c_class)
    g.width g.height g.x g.y in
  Printf.printf "Info: %s" s;
  print_newline ();
  s

let rec query s w =
  let qt = X.queryTree display w in
  let g = X.getGeometry display w in
  Printf.printf "Window: %d %s (%d,%d,%d x %d) [" (window_to_id w)
  (try let (c,_) = Wintbl.find s.s_clients w in c.c_name with _ -> "")
  g.gg_x g.gg_y g.gg_width g.gg_height; print_newline ();
  List.iter (query s) qt.qt_subwindows;
  Printf.printf "]"; print_newline ()
  
let print_all sw =
  Printf.printf "All clients : --------------------------------------\n";
  Wintbl.iter (fun win (c,w) ->
      if win = c.c_window then
        begin
          Printf.printf "-------------  Window %d\n" (window_to_id win);
          Printf.printf "Class %s.%s\n" (fst c.c_class) (snd c.c_class);
          Printf.printf "Name : %s\n"  c.c_name;
          (if c.c_transient_for <> noWindow then Printf.printf "Transient for %d\n" (window_to_id c.c_transient_for));
          (match c.c_wm_state with
              WithdrawnState -> Printf.printf "Withdrawn\n"  
            | IconicState -> Printf.printf "Iconic\n"
            | NormalState -> Printf.printf "Normal\n"
            | _ -> ()
          );
          Printf.printf "Wm_size_hints :\n";
          let nh = c.c_size_hints in
          Printf.printf "Position by %s, size by %s\n"
            (if nh.user_position then "user" else
            if nh.program_position then "program" else "?")
          (if nh.user_size then "user" else
            if nh.program_size then "program" else "?");
          (match nh.min_size with
              None -> ()
            | Some (dx,dy) -> Printf.printf "Min size : %dx%d\n" dx dy);
          (match nh.max_size with
              None -> ()
            | Some (dx,dy) -> Printf.printf "Max size : %dx%d\n" dx dy);
          (match nh.resize_inc with
              None -> ()
            | Some (dx,dy) -> Printf.printf "Resize inc : %dx%d\n" dx dy);
          (match nh.base_size with
              None -> ()
            | Some (dx,dy) -> Printf.printf "Base size : %dx%d\n" dx dy);
          (match nh.aspect with
              None -> ()
            | Some (a,b,c,d) -> Printf.printf "Aspect : %d,%d,%d,%d\n"
                  a b c d);
          Printf.printf "Wm Hints :\n";
          let wh = c.c_wm_hints in
          if wh.urgency_hint then Printf.printf "Urgent\n";
          (match wh.input with
              None -> () | Some b ->
                Printf.printf "Input focus : %s\n" 
                  (if b then "Active" else "Passive"));
          (match wh.initial_state with None -> ()
            | Some s -> Printf.printf "Initial state : %s\n"
                  (if s = IconicState then "Iconic" else
                  if s = NormalState then "Normal" else
                    "Withdrawn"));
          if wh.icon_pixmap <> noWindow then
            Printf.printf "Icon pixmap specified\n";
          if wh.icon_window <> noWindow then
            Printf.printf "Icon Window specified\n";
          if wh.window_group <> noWindow then
            Printf.printf "In window group\n";
          if c.c_take_focus then Printf.printf "Take focus\n";
          if c.c_delete_window then Printf.printf "Delete window\n";
          (if c.c_colormap <> noColormap then
              Printf.printf "Own colormap\n");
          query sw.w_screen w.w_top.w_window
        end;
  ) sw.w_screen.s_clients;
  Printf.printf "----------------------------------------------------";
  print_newline () 

let rlogin station w =
  let _ = command (Printf.sprintf "(xterm -e rlogin %s)&" station) in ()
  
let commandw cmd w =
  let _ = command (Printf.sprintf "(%s)&" cmd)  in ()

let (popup1 : menu ref) = ref []

class type menu_manager = object 
  method menu : wob -> int -> int -> int -> menu -> wob
  method reset : wob -> unit
end

class no_menu = (object
  method menu w x y button menu = raise Not_found
  method reset sw = ()
end : menu_manager)

let menu_manager = ref (new no_menu)

  (*
  let menu_mode = ref (fun (w : wob) (x : int) (y : int) (button : int) (menu : menu) -> 
      (raise Not_found : wob))
*)

let screen_menu_x = define_option ["screen_menu_x"] "" int_option 5
let screen_menu_y = define_option ["screen_menu_y"] "" int_option 5
let window_menu_x = define_option ["screen_menu_x"] "" int_option 0
let window_menu_y = define_option ["screen_menu_y"] "" int_option 0

let screenp w = w.w_top.w_parent == w
  
let popup_menu target from_event menu = 
  let w = target in
  let qp = X.queryPointer display w.w_screen.s_scr.scr_root in
  let s = w.w_screen in
  let (root_x,root_y,button) =
    (* if from_event then *)
    match Eloop.last_event s.s_scheduler with
    | ButtonPressEvent e ->  
        e.Xbutton.x_root, e.Xbutton.y_root, e.Xbutton.detail
    | ButtonReleaseEvent e ->  
        e.Xbutton.x_root, e.Xbutton.y_root, e.Xbutton.detail
    | KeyPressEvent _ | KeyReleaseEvent _ ->  
        if screenp w then
          (!!screen_menu_x, !!screen_menu_y, 0)
        else
        let g = w.w_top.w_geometry in
        (!!window_menu_x + g.x, !!window_menu_y + g.y, 0)
    | _ -> 
        qp.qp_root_x, qp.qp_root_y, 0
    (* else qp.qp_root_x, qp.qp_root_y, 0 *)
  in
  let button = if qp.qp_modifiers = 0 then 0 else button in
  (!menu_manager)#menu target root_x root_y button menu
  
let place_menu target root_x root_y button menu = 
  (!menu_manager)#menu target root_x root_y button menu
  
  
let winmenu sw =
  let s = sw.w_screen in
  let list = ref [] in
  Wintbl.iter (fun win (c,w) ->
      if win = c.c_window then
        list := (c,w) :: !list) s.s_clients;
  List.map (fun (c,w) ->
      (Printf.sprintf "%s : %s" (fst c.c_class) c.c_name),[],
      Function (fun sw ->
          if c.c_wm_state = IconicState then
            (try Wob.send w.w_top (WobDeiconifyRequest true) with _ -> ());
          goto_window w.w_top)
  ) !list

let client = Wobenv.new_var ()
  
let deiconify_last _ =
  match !last_iconify_windows with
    [] -> ()
  | head :: tail -> 
      last_iconify_windows := tail;
      Wob.send head (WobDeiconifyRequest true)

let (menus_table : (string, menu) Hashtbl.t) =  Hashtbl.create 13 
let (funs_table : (string, func) Hashtbl.t) = Hashtbl.create 13
  
let keysym string = 
  try List.assoc string XK.name_to_keysym with
    _ -> Printf.printf "Unkown key binding <%s>" string; print_newline ();
      raise Not_found

open Parameters

let title_icon_var = Wobenv.new_var ()
let sticky_var = Wobenv.new_var ()
let ontop_var = Wobenv.new_var ()

let define_class_option list_name help option list_value  =
  let var = Wobenv.new_var () in
  let get c = find_option c var in
  let set (classe, name) value = 
    let name = 
      if name = "" then  classe else
      if classe = "" then name else
        classe^"."^name
    in
    add_option name var value
  in
  let list = define_option [list_name] help
      (list_option (tuple2_option (option, list_option string2_option)))
    list_value
  in
  List.iter (fun (v, list) ->
      List.iter (fun p -> set p v) list
  ) !!list;
  (var, get, set, list)

let define_bool_option list_name list_value  =
  let var = Wobenv.new_var () in
  let get c = find_option c var in
  let set (classe, name) = 
    let name = 
      if name = "" then  classe else
      if classe = "" then name else
        classe^"."^name
    in
    add_option name var true
  in
  let list = define_option [list_name] "" (list_option  string2_option)
    list_value
  in
  add_option "" var false;
  List.iter (fun p -> set p) !!list;
  (var, get, set, list)

let onbottom_var = Wobenv.new_var ()
  
let gwml_ontop_windows = ref []
  
let is_on_top c =
  try find_option c ontop_var with _ -> false
        
let add_is_on_top (classe, name) =
  let name = 
    if name = "" then  classe else
    if classe = "" then name else
      classe^"."^name
  in
  add_option name ontop_var true

let is_on_bottom c =
  try find_option c onbottom_var with _ -> false
        
let add_is_on_bottom (classe, name) =
  let name = 
    if name = "" then  classe else
    if classe = "" then name else
      classe^"."^name
  in
  add_option name onbottom_var true
  
let find_pixmap filename =
  Utils.find_in_path !!graphics_path filename
      
let add_title_icon pixmap (classe, name) =
  let name = 
    if name = "" then  classe else
    if classe = "" then name else
      classe^"."^name
  in
  add_option name title_icon_var pixmap
  
let mini_icons = define_option ["mini_icons"] "" 
    (list_option (list_option string2_option)) []
  
let _ =
  List.iter (fun list ->
      match list with
        [] -> ()
      | (pixmap, _) :: tail ->
          List.iter (fun c -> add_title_icon pixmap c) tail
  ) !!mini_icons
  
  
let get_title_icon c =
  try 
    let filename = (Xrm.get x_res
          ((fst c.c_class):: (snd c.c_class) :: ["title_pixmap"]))
    in
    find_pixmap filename
  with _ -> 
      try
        let filename = (Printf.sprintf "mini-%s.xpm"
            (String.lowercase (fst c.c_class)))
        in
        find_pixmap filename
      with _ -> 
          try 
            let filename = find_option c title_icon_var in
            find_pixmap filename
          with _ ->
              let filename = "mini-redhat.xpm" in
              find_pixmap filename
    
let rec manage_options w options =
  match options with
    [] -> ()
  | option :: options ->
      manage_options w options;
      match option with
        Placement placement -> ()
      | Decoration decoration -> ()
      | Icon icon_name -> ()
      | BorderWidth int -> ()
      | HandleWidth int -> ()
      | NoTitle -> ()
      | Sticky -> Wob.setenv w sticky_var true
      | WindowListSkip -> ()
      | StaysOnTop -> ()
      | ClickToFocus -> ()
      | NoHandles -> ()
      | StartsOnDesk int -> ()
      | TitleIcon string -> ()
      | NoPPosition  -> ()
      | SmartPlacement -> ()
      | RandomPlacement -> ()
      | StubbornPlacement -> ()
      | SloppyFocus -> ()
      | NoIcon -> ()
      | MWMDecor -> ()
      | MWMFunctions -> ()
      | HintOverride -> ()
      | DecorateTransient -> ()
      | CirculateSkip -> ()
      | ForeColor string -> ()
      | BackColor string -> ()
      | NoButton int -> ()
      | Color (fg,bg) -> ()
      | StartIconic -> ()
  
  
let _ =
  decorate_client := (fun sw c_wob ->
          let c = c_wob#client in
          let (classe,name) = c.c_class in
          let deco = find_decoration c in
          let (hooks, left, right, top, bottom) = deco sw c in
          let top =     
            Top.make sw "" hooks c_wob left right top bottom in 
          top#set_client c;
          Wob.setenv top#wob client c;
          Log.catch "decorate_client: WobMapRequest %s"
            (fun _ -> Wob.send top#wob WobMapRequest);
      )

  (* This function should not be used. Not ready yet.
  As soon as the client has been redecorated, it is unmapped and
  withdrawn. This could be caused by the events which are received 
  after by the screen handler.
  *)
let redecorate w =
  let c = w.w_oo#client in
  let s = w.w_screen in
  let (c,w) = Wintbl.find s.s_clients c.c_window in
  let cw = w.w_oo in
  let tw = w.w_top in
  let tg = tw.w_geometry in
  let sw = tw.w_parent in
  Xsync.reparentWindow display c.c_window sw.w_window tg.x tg.y;
  Wob.send tw WobDestroy;
  Xlib.removePredEvent display (fun ev ->
      match ev.ev_event with
        UnmapNotifyEvent e when e.Xunmap.window = c.c_window -> true
      | _ -> false);
  let c = Client.create sw c.c_window false in
  let (c,w) = Wintbl.find s.s_clients c.c_window in
  wm_broadcast (c, w,AddClient)
  
let goto_win_avoid = define_option [ "goto_win_avoid" ] ""
    (list_option string_option) [
    "GwML"; "XConsole"; "XClock"; "Clock";"xcpustate";"xosview";"xload";
    "XBuffy";
  ]
  
let distance x1 y1 x2 y2 =
  let dx = x1-x2 in
  let dy = y1-y2 in
  dx * dx + dy * dy
          
let rec goto_win_aux win_choice sw closest = 
(* take the list of windows on the current screen *)
  let screen = sw.w_screen in
  let s = screen.s_scr in
  let qp = X.queryPointer display s.scr_root in 
  let best = ref None in
  Wintbl.iter (fun win (c,w) ->
      if c.c_wm_state = NormalState && 
        not (List.mem (fst c.c_class) !!goto_win_avoid) &&
        not (List.mem (snd c.c_class) !!goto_win_avoid)
      then
        let tw = w.w_top in
        let g = tw.w_geometry in
        if g.x >= -20 && g.x + 15 <= s.scr_width &&
          g.y >= -20 && g.y + 15 <= s.scr_height then
          win_choice tw qp best
  ) screen.s_clients;
  match !best with
    None -> if not closest then goto_closest sw
  | Some (tw, x, y) ->
      X.warpPointer display s.scr_root 0 0 s.scr_width s.scr_height
        tw.w_window 7 7;
      Wob.send  tw WobRaiseWindow

and goto_closest sw =
  let dist = ref 0 in
  goto_win_aux (fun w qp best ->
      let tw = w.w_top in
      let g = tw.w_geometry in
      let x = g.x in
      let y = g.y in
      match !best with
        None -> 
          best := Some (tw, x, y);
          dist := distance qp.qp_root_x qp.qp_root_y x y;
      | Some (w, xx, yy) ->
          let new_dist = distance qp.qp_root_x qp.qp_root_y x y in
          if new_dist < !dist then begin
              best := Some (tw, x, y);
              dist := new_dist;
            end
  ) sw true
  
let goto_win win_choice sw = goto_win_aux win_choice sw false
      
let message_win = (Wobenv.new_var () : Label.label Wobenv.var)

let message_delay = define_option ["message_delay"] "" float_option 5.0
let message sw msg =
  let message_removed = ref false in
  let message_win = try
      let label = Wob.getenv sw message_win in
      label#set_string msg;
      label
    with Not_found ->
        let label = new Label.label msg in
        let message_hook top e =
          match e with
            WobKeyPress _ -> 
              message_removed := true;
              Wob.send label#wob.w_top (WobUnmap false)
          | _ -> ()
        in
        label#set_min_width 20;
        let top = Top.make sw "" [message_hook] (label :> wob_desc) 
          None None None None in
        let tw = top#wob in
        let g = tw.w_geometry in
        let scr = sw.w_screen.s_scr in
        g.x <- scr.scr_width / 2;
        g.y <- 5;
        Wob.setenv sw message_win label;
        label
  in  
  Wob.send message_win#wob.w_top WobMap;
  Concur.Thread.add_timer !!message_delay (fun _ ->
      if not !message_removed then
        Wob.send message_win#wob.w_top (WobUnmap false)
  )

let rec simplify_list list res =
  match list with
    [] -> res
  | v :: tail ->
      simplify_list tail (if List.mem v res then res else v :: res)
  
let list_clients w =
  let list = ref [] in
  Wintbl.iter (fun win (c,w) ->
      list := (c,w) :: !list) w.w_screen.s_clients;
  simplify_list !list []

let iconified = Wobenv.new_var ()
  
let set_stay_iconified w value = 
  Wob.setenv w iconified (value : bool)

let stay_iconified w = 
  Wob.getenv w iconified
  
class type undo_manager = object
  method undo : unit
  method add_undo : (unit -> unit) -> unit
end

class no_undo_manager =
  (object
  method undo = ()
  method add_undo f = ()
end : undo_manager)

let undo_manager = ref (new no_undo_manager)
  
      
let deiconify_sound = define_option ["deiconify_sound"] "" filename_option ""
let iconify_sound = define_option ["iconify_sound"] "" filename_option ""
let exit_sound = define_option ["exit_sound"] "" filename_option ""
let hide_sound = define_option ["hide_sound"] "" filename_option ""
let maximize_sound = define_option ["maximize_sound"] "" filename_option ""
let shade_sound = define_option ["shade_sound"] "" filename_option "" 
let start_sound = define_option ["start_sound"] "" filename_option "" 
let startup_sound = define_option ["startup_sound"] "" filename_option "" 
let unhide_sound = define_option ["unhide_sound"] "" filename_option "" 
let unshade_sound = define_option ["unshade_sound"] "" filename_option "" 
let unmaximize_sound = define_option ["unmaximize_sound"] "" filename_option ""
  
    
class q_ledit cont =
  object (self)

  val mutable cont = cont
    
  method set_cont c = cont <- c
  inherit Ledit.ledit ""
  
  method edit_set_string = 
    let tw = self#wob.w_top in
    (try Wob.send tw (WobUnmap true) with _ -> ());
    (try cont self#string with _ -> ())
    
end
    
let question_win = (Wobenv.new_var () : (Label.label * q_ledit) Wobenv.var)

let question sw msg default cont =
  let sw = sw.w_top.w_parent in
  let message_removed = ref false in
  let label, ledit = try Wob.getenv sw question_win with Not_found ->
        let label = new Label.label "" in
        let ledit = new q_ledit cont in
        label#set_min_width 30;
        ledit#set_min_width 100;
        label#set_min_height 40;
        ledit#set_min_height 40;
        let bar = Bar.make Horizontal [| Wob.desc label; Wob.desc ledit |] in
        let top = Top.make sw "" [] (bar :> wob_desc) None None None None in
        let tw = top#wob in
        let g = tw.w_geometry in
        let scr = sw.w_screen.s_scr in
        g.x <- scr.scr_width / 2;
        g.y <- 5;
        Wob.setenv sw question_win (label, ledit);
        label, ledit
  in  
  label#set_string msg;  
  ledit#set_string default;
  ledit#set_cont cont;
  Wob.send ledit#wob.w_top WobMap

let name_to_keysym = 
  ("Button1", XK.xk_Pointer_Button1) ::
  ("Button2", XK.xk_Pointer_Button2) ::
  ("Button3", XK.xk_Pointer_Button3) ::
  ("Button4", XK.xk_Pointer_Button4) ::
  ("Button5", XK.xk_Pointer_Button5) ::
  ("DblClick1", XK.xk_Pointer_DblClick1) ::
  ("DblClick2", XK.xk_Pointer_DblClick2) ::
  ("DblClick3", XK.xk_Pointer_DblClick3) ::
  ("DblClick4", XK.xk_Pointer_DblClick4) ::
  ("DoubleClick5", XK.xk_Pointer_DblClick5) ::
  ("DoubleClick1", XK.xk_Pointer_DblClick1) ::
  ("DoubleClick2", XK.xk_Pointer_DblClick2) ::
  ("DoubleClick3", XK.xk_Pointer_DblClick3) ::
  ("DoubleClick4", XK.xk_Pointer_DblClick4) ::
  ("DoubleClick5", XK.xk_Pointer_DblClick5) ::
  ("DeltaMove1", XK.xk_Pointer_Drag1) ::
  ("DeltaMove2", XK.xk_Pointer_Drag2) ::
  ("DeltaMove3", XK.xk_Pointer_Drag3) ::
  ("DeltaMove4", XK.xk_Pointer_Drag4) ::
  ("DeltaMove5", XK.xk_Pointer_Drag5) ::
  ("DMovee1", XK.xk_Pointer_Drag1) ::
  ("DMove2", XK.xk_Pointer_Drag2) ::
  ("DMove3", XK.xk_Pointer_Drag3) ::
  ("DMove4", XK.xk_Pointer_Drag4) ::
  ("DMove5", XK.xk_Pointer_Drag5) ::
  XK.name_to_keysym
  
let value_to_keysym v =
  match v with
    Value v -> List.assoc v name_to_keysym
  | _ -> raise Not_found
let keysym_to_value k =
  Value (List.assoc k XK.keysym_to_name)
  
  (* form: SC-Button1 *)
let value_to_key v =
  match v with 
    Value s -> 
      let key, mods = 
        try
          let index = String.index s '-' in
          let mods = String.sub s 0 index in
          let key = String.sub s (index+1) (String.length s - index - 1) in
          key, mods
        with _ -> s, ""
      in
      let key = List.assoc key name_to_keysym in
      let mods = string_to_modifier mods in
      if key = XK.xk_Pointer_Button1 then Button(1 , mods) else
      if key = XK.xk_Pointer_Button2 then Button(2 , mods) else
      if key = XK.xk_Pointer_Button3 then Button(3 , mods) else
      if key = XK.xk_Pointer_Button4 then Button(4 , mods) else
      if key = XK.xk_Pointer_Button5 then Button(5 , mods) else
      
      if key = XK.xk_Pointer_DblClick1 then DblClick(1 , mods) else
      if key = XK.xk_Pointer_DblClick2 then DblClick(2 , mods) else
      if key = XK.xk_Pointer_DblClick3 then DblClick(3 , mods) else
      if key = XK.xk_Pointer_DblClick4 then DblClick(4 , mods) else
      if key = XK.xk_Pointer_DblClick5 then DblClick(5 , mods) else
      
      
      if key = XK.xk_Pointer_Drag1 then BMove(1 , mods) else
      if key = XK.xk_Pointer_Drag2 then BMove(2 , mods) else
      if key = XK.xk_Pointer_Drag3 then BMove(3 , mods) else
      if key = XK.xk_Pointer_Drag4 then BMove(4 , mods) else
      if key = XK.xk_Pointer_Drag5 then BMove(5 , mods) else

        Key (key, mods)
        
  | _ -> raise Not_found
  
let key_to_string k =
  let s =
    match k with
      Key (keycode, mods) -> 
        let mods = modifier_to_string mods in
        (if mods = "" then "" else mods ^ "-") ^ 
          (List.assoc keycode XK.keysym_to_name)
    | Button (detail, mods) ->
        let mods = modifier_to_string mods in
        (if mods = "" then "" else mods ^ "-") ^ 
          (Printf.sprintf "Button%d" detail)
    | DblClick (detail, mods) ->
        let mods = modifier_to_string mods in
        (if mods = "" then "" else mods ^ "-") ^ 
          (Printf.sprintf "DblClick%d" detail)
    | BMove (detail, mods) ->
        let mods = modifier_to_string mods in
        (if mods = "" then "" else mods ^ "-") ^ 
          (Printf.sprintf "DeltaMove%d" detail)
  in s      

let key_to_value k = Value (key_to_string k)
  
let key_option = define_option_class "Key" value_to_key key_to_value
  
let value_to_action v =
  match v with Value s -> NamedAction s
  | List ((Value s) :: args) -> NamedActionX (s, List.map value_to_string args)
  | _ -> raise Not_found
      
let action_to_value action =
  match action with NamedAction s -> Value s
  | NamedActionX (s,args) -> SmallList (List.map string_to_value (s::args))
  | _ -> raise Not_found
  
let action_option = define_option_class "Action" value_to_action action_to_value

let value_to_binding v =
  match v with
    List [key; grabbed; action] ->
      (value_to_key key, value_to_action action, value_to_bool grabbed)
  | List [key ; action] ->
      (value_to_key key, value_to_action action, true)
  | _ -> raise Not_found
  
let binding_to_value (key, action, grabbed) =
  SmallList [key_to_value key; bool_to_value grabbed; action_to_value action]

type binding = Gwml.binding * action * bool
  
let binding_option = define_option_class "Binding" 
  value_to_binding binding_to_value
  
let add_action option ((key, action, grabbed) as binding) =
  try
    List.iter (fun (key_o, action_o, grabbed_o) ->
        if key_o = key && grabbed_o = grabbed then raise Exit
    ) !!option;
    option =:= binding :: !!option
  with _ -> ()
      
let add_actions option bindings = List.iter (add_action option) bindings
  
  
let screen_actions = define_option ["screen_actions"] 
  "<screen_actions> are the bindings in the root window. The format is 
  a list of triples (event, grabbed, action) where <event> is formatted as
  [modifiers* -] (key | Button[0-2] | DblClick[0-2] | DeltaMove[0-2]),
  <grabbed> is <true> is the key should be grabbed in all windows, or
  <false> is ony used in the root window, and <action> is the action
  associated with the event. The modifiers are C (Control), S (Shift),
  A (any), M or 1 (Alt) and 2-5 for other ones. An empty list means
  <use default bindings>."
    (list_option binding_option) []
let title_actions = define_option ["title_actions"] "" 
    (list_option binding_option) [] 
let window_actions = define_option ["window_actions"] 
  "<window_actions> are the bindings used in a client window. The format is 
  the same as <screen_actions>." 
    (list_option binding_option) []
  
let execute_action action w =
  match get_action action with
    NoAction -> ()
  | Function f -> f w
  | Menu m -> ignore (popup_menu w true (m ()))
  | ActiveMenu m -> ignore (popup_menu w true (m w))
  | NamedAction _ -> assert false
  | NamedActionX _ -> assert false
      
let convert_bindings l =
  List.map (fun (key, action, grabbed) ->
      (key, execute_action action, grabbed)) l
  
  
let createMenuIcon w =
  let s = w.w_screen in
  let h = 15 in
  let scr = s.s_scr in
  let pix = X.createPixmap display scr.scr_root h h 1 in
  let gc = X.createGC  display pix [GCforeground (id_to_pixel 0)] in
  fillRectangle display pix gc 0 0 h h;
  setForeground display gc (id_to_pixel 1);
  X.fillPoly display pix gc Convex Origin [2,2; h-2,h/2; 2,h-2];
  X.freeGC display gc;
  pix, noPixmap  

let submenu_item =
  ItemPixmap (FromFunction ("submenu_pixmap", createMenuIcon),false)
  
let menuitem_to_value (name, _, action) =
  let item = match get_action action with
      ActiveMenu _ | Menu _ -> "M"
    |  _ -> ""
  in SmallList [Value name; Value item; action_to_value action]

let value_to_menuitem v =
  match v with
    List [Value name; v] -> 
      let action = value_to_action v in
      let item = match get_action action with
          ActiveMenu _ | Menu _ -> [submenu_item]
        | _ -> []
      in
      (name, item, action)
  | List [Value name; Value ""; v] -> 
      let action = value_to_action v in
      (name, [], action)
  | List [Value name; Value "M"; v] ->
      let action = value_to_action v in
      (name, [submenu_item], action)
  | _ -> raise Not_found
      
let menuitem_option = define_option_class "MenuItem"
    value_to_menuitem menuitem_to_value
  
let menu_option = list_option menuitem_option
    
let _ =
  if !!Dyneval.libraries = [] then
    Dyneval.libraries =:= [
      "stdlib.cma", [
        "Arg";       "Filename";  "Lexing";   "Parsing";   "Set";
        "Sys";
        "Array";     "Format";    "List";     "Pervasives";  "Sort"; 
        "Weak";
        "Buffer";    "Gc";        "Map";      "Printexc";    "Stack";
        "Callback";  "Genlex";    "Marshal";  "Printf";      
        "Char";      "Hashtbl";   "Obj";      "Queue";       "Stream";
        "Digest";    "Lazy";      "Oo";       "Random";      "String";
      ];
      "WXlib.cma", [
        "WX_Graphics";  "WX_display";   "WX_popup";        "WX_table";
        "WX_adjust";    "WX_dummy";     "WX_port";         "WX_text";
        "WX_appli";     "WX_filesel";   "WX_radiobutton";  "WX_top";
        "WX_bar";       "WX_label";     "WX_root";         "WX_tree";
        "WX_base";      "WX_ledit";     "WX_scale";        "WX_types";
        "WX_button";    "WX_notebook";  "WX_screen";       "WX_viewport";
        "WX_config";    "WX_object";    "WX_scrollbar";    "WX_wmtop";
        "WX_deleg";     "WX_panel";     "WX_selector";     "WX_xterm";
        "WX_dialog";    "WX_pixmap";    "WX_swap"; ]; 
      "xlib.cma", [
        "Selection" 
      ]
    ]

    (*
  La liste represente l ordre des fenetre dans la pile du serveur
  
    *)
    
let stack = ref []

let ontop_update_do = ref false
  
let ontop_update tw =
  if !ontop_update_do then
    let list = list_clients tw in
    ontop_update_do := false;
    List.iter (fun (c,w) ->
        if not (c.c_transient_for == noWindow) then
          try
            let s = w.w_screen in
            let (_,ww) = Wintbl.find s.s_clients c.c_transient_for  in
            X.configureWindow display w.w_top.w_window [
              CWSibling ww.w_top.w_window; CWStackMode Above]
          with _ -> ()
    ) list;
    List.iter (fun (c,w) ->
        if Wob.sgetenv w.w_top ontop_var false then
          Xlib.raiseWindow display w.w_top.w_window
        else
        if Wob.sgetenv w.w_top onbottom_var false then
          Xlib.lowerWindow display w.w_top.w_window
    ) list;
    List.iter (Xlib.raiseWindow display) !gwml_ontop_windows
    
let stack_remove w =
  stack := Utils.list_removeq !stack w

let top_stack_p w =
  let w = w.w_top in
  match !stack with
    ww :: _ when ww = w -> true
  | _ -> false

let bottom_stack_p w =
  let rec iter list w =
    match list with [] -> false
    | [ ww ] when w == ww -> true
    | ww :: tail when w == ww -> false
    | ww :: tail -> iter tail w
  in
  iter !stack w.w_top

let ontop_update tw =
  if not !ontop_update_do then begin
      Wob.after_events (fun _ -> ontop_update tw);
      ontop_update_do := true
    end
            
let stack_raise w =
  if not (top_stack_p w) then
    begin
      let w = w.w_top in
      stack_remove w;
      Xlib.raiseWindow display w.w_top.w_window;
      stack := w :: !stack;
      ontop_update w
    end

let stack_lower w =
  if not (bottom_stack_p w) then
    begin
      let w = w.w_top in
      stack_remove w;
      Xlib.lowerWindow display w.w_top.w_window;
      stack := !stack @ [w];
      ontop_update w      
    end
  