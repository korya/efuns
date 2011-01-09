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

type item_option =
    ItemPixmap of (Gwml.pixmap_desc * bool)
  | ItemForeground of (unit -> string)
  | ItemBackground of (unit -> string)
  | ItemFont of (unit -> string)
  | ItemHiliteForeground of (unit -> string)
  | ItemHiliteBackground of (unit -> string)
  | ItemHiliteFont of (unit -> string)
  | ItemTitle
and menu = (string * item_option list * action) list
and action =
    NoAction
  | Menu of (unit -> menu)
  | ActiveMenu of (Gwml.wob -> menu)
  | Function of func
  | NamedAction of string
  | NamedActionX of string * string list
and func = Gwml.wob -> unit
val smart_install : bool Options.option_record
val value_to_image : Options.option_value -> Gwml.image
val image_to_value : Gwml.image -> Options.option_value
val image_option : Gwml.image Options.option_class
val root_image : Gwml.image Options.option_record
val root_background : string Options.option_record
val active_background : string Options.option_record
val active_foreground : string Options.option_record
val active_font : string Options.option_record
val active_image : Gwml.image Options.option_record

val iconMgr_active_background : string Options.option_record
val iconMgr_active_foreground : string Options.option_record
val iconMgr_active_font : string Options.option_record
val iconMgr_active_image : Gwml.image Options.option_record
val iconMgr_title_background : string Options.option_record
val iconMgr_title_foreground : string Options.option_record
val iconMgr_title_font : string Options.option_record
val iconMgr_title_image : Gwml.image Options.option_record
val editable_title : bool Options.option_record
val do_animation : bool Options.option_record
val pan_on_click : bool Options.option_record
val confine_move : bool Options.option_record
val opaque_move : bool Options.option_record
val grab_server : bool Options.option_record
val auto_raise : bool Options.option_record
val auto_colormap : bool Options.option_record
val group_iconification : bool Options.option_record
val windows_corner : bool Options.option_record
val debug_command : bool Options.option_record
val resize_font : string Options.option_record
val window_font : string Options.option_record
val window_background : string Options.option_record
val window_foreground : string Options.option_record
val title_background : string Options.option_record
val title_image : Gwml.image Options.option_record
val title_foreground : string Options.option_record
val title_font : string Options.option_record
val sticky_foreground : string Options.option_record
val sticky_background : string Options.option_record
val justification_option : Gwml.justified Options.option_class
val title_justification : Gwml.justified Options.option_record
val edge_scrolling_vertic : int Options.option_record
val edge_scrolling_horiz : int Options.option_record
val edge_scrolling_resist : int Options.option_record
val edge_moving_resist : float Options.option_record
val opaque_move_size : int Options.option_record
val iconMgr_foreground : string Options.option_record
val iconMgr_background : string Options.option_record
val iconMgr_font : string Options.option_record
val iconMgr_image : Gwml.image Options.option_record
val icon_foreground : string Options.option_record
val icon_background : string Options.option_record
val icon_font : string Options.option_record
val icon_borderwidth : int Options.option_record
val frame_borderwidth : int Options.option_record
val menu_font : string Options.option_record
val menu_foreground : string Options.option_record
val menu_background : string Options.option_record
val menu_image : Gwml.image Options.option_record
val menu_hilite_font : string Options.option_record
val menu_hilite_foreground : string Options.option_record
val menu_hilite_background : string Options.option_record
val menu_hilite_image : Gwml.image Options.option_record
val menu_title_image : Gwml.image Options.option_record
val menu_title_font : string Options.option_record
val menu_title_foreground : string Options.option_record
val menu_title_background : string Options.option_record
val panner_foreground : string Options.option_record
val panner_background : string Options.option_record
val auto_raise_delay : float Options.option_record
val animation_delay : float Options.option_record
val prevent_autoraise : bool ref
val is_iconified_var : bool Wobenv.var
val is_in_dvroom_var : bool Wobenv.var
val is_group_iconified_var : bool Wobenv.var
val is_minimized_var : int Wobenv.var
val focus_var : bool Wobenv.var
val icon_var : (Top.top * Xtypes.window list ref) Wobenv.var
val old_width : (int ref * int ref) Wobenv.var
val old_height : (int ref * int ref) Wobenv.var
val command : string -> int
val comp_name : (string -> string) ref
val modifiers_option : int Options.option_class
val wm_modifiers : int Options.option_record
val icon_path : string list Options.option_record
val last_iconify_windows : Gwml.wob list ref
type pager =
  { pager_window: Xtypes.window;
    pager_width: int;
    pager_height: int }
class type virtual_manager =
  object
    method add_hook : Gwml.wob -> (Gwml.wob -> unit) -> unit
    method current_position : Gwml.wob -> int * int
    method drawp : Gwml.wob -> bool
    method goto : Gwml.wob -> unit
    method move : Gwml.wob -> int -> int -> unit
    method movep : Gwml.wob -> bool
    method omit_draw : Gwml.wob -> bool -> unit
    method omit_move : Gwml.wob -> bool -> unit
    method pager : Gwml.wob -> pager
    method place_on_screen : Gwml.wob -> unit
    method start : Gwml.wob -> unit
    method update : Gwml.wob -> unit
  end
class no_virtual : virtual_manager
val virtual_manager : virtual_manager ref
val toggle_virtual_move : Gwml.wob -> unit
class type icon_manager =
  object
    method create :
      Gwml.wob ->
      int -> int -> string -> int -> (Gwml.client_desc -> bool) -> unit
    method hook : Gwml.client_desc -> Gwml.wob_desc -> Gwml.wob_event -> unit
    method message : Gwml.wob -> string -> unit
  end
class no_iconMgr : icon_manager
val resize_mode : (Gwml.wob -> bool -> unit) ref
val resize : Gwml.wob -> bool -> unit
val icon_manager : icon_manager ref
val icon_manager_hook : Gwml.client_desc -> Top.top -> Gwml.wob_event -> unit
type decoration =
  Gwml.wob ->
  Gwml.client_desc ->
  (Top.top -> Gwml.hook) list * Gwml.wob_desc option * Gwml.wob_desc option *
  Gwml.wob_desc option * Gwml.wob_desc option
and placement = Gwml.client_desc -> Gwml.wob -> unit
module Parameters :
  sig
    type t =
        Placement of placement
      | Decoration of decoration
      | Icon of string
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
val name_options : Wobenv.env Opttable.t
val class_options : Wobenv.env Opttable.t
val complete_options : Wobenv.env Opttable.t
val std_options_var : Parameters.t list Wobenv.var
val add_std_options : string -> Parameters.t list -> unit
val add_option : string -> 'a Wobenv.var -> 'a -> unit
val add_placement : placement -> string * string -> unit
val add_icon : string -> string * string -> unit
val add_decoration : decoration -> string * string -> unit
val find_std_options : string -> string -> Parameters.t list
val find_option : Gwml.client_desc -> 'a Wobenv.var -> 'a
val find_placement : Gwml.client_desc -> placement
val find_icon : Gwml.client_desc -> string
val find_decoration : Gwml.client_desc -> decoration
val modulo : int -> int -> int
val div : int -> int -> int
val goto_window : Gwml.wob -> unit
val x_maximize_use : int Options.option_record
val y_maximize_use : int Options.option_record
val maximize_x : Gwml.wob -> unit
val maximize_y : Gwml.wob -> unit
val map_all : Gwml.wob -> unit
val info : Gwml.client_desc -> Gwml.wob -> string
val print_all : Gwml.wob -> unit
val rlogin : string -> 'a -> unit
val commandw : string -> 'a -> unit
val popup1 : menu ref
class type menu_manager =
  object
    method menu : Gwml.wob -> int -> int -> int -> menu -> Gwml.wob
    method reset : Gwml.wob -> unit
  end
class no_menu : menu_manager
val menu_manager : no_menu ref
val popup_menu : Gwml.wob -> 'a -> menu -> Gwml.wob
val place_menu : Gwml.wob -> int -> int -> int -> menu -> Gwml.wob
val winmenu : Gwml.wob -> (string * 'a list * action) list
val client : Gwml.client_desc Wobenv.var
val deiconify_last : 'a -> unit
val menus_table : (string, menu) Hashtbl.t
val funs_table : (string, func) Hashtbl.t
val keysym : string -> int
  val title_icon_var : string Wobenv.var
  val add_title_icon : string -> (string * string) -> unit
val sticky_var : bool Wobenv.var
val ontop_var : bool Wobenv.var
val is_on_top : Gwml.client_desc -> bool
val add_is_on_top : string * string -> unit
val onbottom_var : bool Wobenv.var
val is_on_bottom : Gwml.client_desc -> bool
val add_is_on_bottom : string * string -> unit
val find_pixmap : string -> string
val get_title_icon : Gwml.client_desc -> string
val manage_options : Gwml.wob -> Parameters.t list -> unit
val goto_win_avoid : string list Options.option_record
val distance : int -> int -> int -> int -> int
val goto_win_aux :
  (Gwml.wob ->
   Xtypes.queryPointerRep -> (Gwml.wob * int * int) option ref -> unit) ->
  Gwml.wob -> bool -> unit
val goto_closest : Gwml.wob -> unit
val goto_win :
  (Gwml.wob ->
   Xtypes.queryPointerRep -> (Gwml.wob * int * int) option ref -> unit) ->
  Gwml.wob -> unit
val message_win : Label.label Wobenv.var
val message_delay : float Options.option_record
val message : Gwml.wob -> string -> unit
val simplify_list : 'a list -> 'a list -> 'a list
val list_clients : Gwml.wob -> (Gwml.client_desc * Gwml.wob) list
val iconified : bool Wobenv.var
val set_stay_iconified : Gwml.wob -> bool -> unit
val stay_iconified : Gwml.wob -> bool
class type undo_manager =
  object method add_undo : (unit -> unit) -> unit method undo : unit end
class no_undo_manager : undo_manager
val undo_manager : no_undo_manager ref
val deiconify_sound : string Options.option_record
val iconify_sound : string Options.option_record
val exit_sound : string Options.option_record
val hide_sound : string Options.option_record
val maximize_sound : string Options.option_record
val shade_sound : string Options.option_record
val start_sound : string Options.option_record
val startup_sound : string Options.option_record
val unhide_sound : string Options.option_record
val unshade_sound : string Options.option_record
val unmaximize_sound : string Options.option_record
class type desk_manager =
  object
    method goto : Gwml.wob -> unit
    method add_to_desk : Gwml.wob -> string -> unit
    method hook : Gwml.client_desc -> Gwml.wob_desc -> Gwml.wob_event -> unit
    method new_desk : Gwml.wob -> string -> unit
    method rename_desk : Gwml.wob -> string -> string -> unit
    method to_desk : Gwml.wob -> string -> unit
  end
class no_desk_manager : desk_manager
val desk_manager : no_desk_manager ref
val desk_manager_hook : Gwml.client_desc -> Top.top -> Gwml.wob_event -> unit
class q_ledit :
  (string -> unit) -> 
  object
    inherit Ledit.ledit
    method set_cont : (string -> unit) -> unit
  end
val question_win : (Label.label * q_ledit) Wobenv.var
val question : Gwml.wob -> string -> string -> (string -> unit) -> unit
val is_viewable : Gwml.wob -> bool
val get_action : action -> action
val define_action : string -> action -> unit
val define_xaction : string -> (string list -> action) -> unit
val action_option : action Options.option_class
type binding = Gwml.binding * action * bool
val binding_option : binding Options.option_class
val add_actions : binding list Options.option_record -> binding list -> unit
val screen_actions : binding list Options.option_record
val title_actions : binding list Options.option_record
val window_actions : binding list Options.option_record
val convert_bindings : binding list -> Gwml.bindings
val submenu_item : item_option
val createMenuIcon : Gwml.wob -> Xtypes.pixmap * Xtypes.pixmap
val menu_option : (
    string * item_option list * action) list Options.option_class
val warp_offset : int Options.option_record
val gwml_ontop_windows : Xtypes.window list ref
val ontop_update : Gwml.wob -> unit
(* val redecorate : Gwml.wob -> unit *)
val define_class_option : string -> string ->
    'a Options.option_class ->
    ('a * (string * string) list) list ->
    'a Wobenv.var * (Gwml.client_desc -> 'a) *
    (string * string -> 'a -> unit) *
('a * (string * string) list) list Options.option_record
val define_bool_option : string ->
    (string * string) list ->
    bool Wobenv.var * (Gwml.client_desc -> bool) *
    (string * string -> unit) * (string * string) list Options.option_record
val execute_action : action -> Gwml.wob -> unit
val stack : Gwml.wob list ref
val stack_remove : Gwml.wob -> unit
val stack_raise : Gwml.wob -> unit
val stack_lower : Gwml.wob -> unit
val redecorate : Gwml.wob -> unit
val top_stack_p : Gwml.wob -> bool
val key_to_string : Gwml.binding -> string