val std_mask : Xtypes.eventMask list
val not_grab_related : Xtypes.notifyMode -> bool
val send : Gwml.wob -> Gwml.wob_event -> unit
val wob_queue : Gwml.wob list ref
val send_one : Gwml.wob -> Gwml.wob_event -> unit
val end_hook : unit -> unit
val default_borderpixel : string Options.option_record
val default_background : string Options.option_record
val default_foreground : string Options.option_record
val default_font : string Options.option_record
val double_click_delay : float Options.option_record
val delta_move_size : int Options.option_record
val last_click : Xtypes.Xbutton.t ref
val click : Gwml.screen_desc -> Gwml.click
val set_grabs :
  Xtypes.display -> Xtypes.window -> (Gwml.binding * 'a * bool) list -> unit
val unset_grabs :
  Xtypes.display -> Xtypes.window -> (Gwml.binding * 'a * bool) list -> unit
val images : (string * int * int, Xtypes.pixmap * Imager.image) Hashtbl.t
val reset_image_cache : unit -> unit
val exit_exn : exn
val make_backimage : Gwml.image -> Gwml.wob -> string -> unit
class wob_base :
  object
    val mutable actions : Gwml.bindings
    val mutable background : string
    val mutable backimage : Gwml.image
    val mutable backimage_pix : Xtypes.pixmap
    val mutable borderpixel : string
    val mutable cursor : Gwml.cursor_desc
    val mutable emask : Xtypes.eventMask list
    val mutable extensible_height : int
    val mutable extensible_width : int
    val mutable font : string
    val mutable foreground : string
    val mutable hilite_background : string
    val mutable hilite_backimage : Gwml.image
    val mutable hilite_foreground : string
    val mutable hilitep : bool
    val mutable is_shaped : bool
    val mutable min_height : int
    val mutable min_width : int
    val mutable render_backimage : bool
    val mutable wob : Gwml.wob option
    val mutable wob_hooks : Gwml.hook list
    val mutable tip : bool
    val mutable inside : bool
    val mutable display_tip :  Gwml.wob_desc -> bool -> unit
    method set_tip_display : (Gwml.wob_desc -> bool -> unit) -> unit
    method actions : Gwml.bindings
    method add_hook : Gwml.hook -> unit
    method background : string
    method backimage : Gwml.image
    method bg : string
    method bgimage : Gwml.image
    method borderpixel : string
    method borderwidth : int
    method client : Gwml.client_desc
    method create : bool -> unit
    method fg : string
    method first_hook : Gwml.wob_event -> unit
    method font : string
    method foreground : string
    method handle_button : Xtypes.Xbutton.t -> unit
    method handle_key : Xtypes.Xkey.t * string * Xtypes.keySym -> unit
    method hilite : unit
    method hilite_background : string
    method hilite_backimage : Gwml.image
    method hilite_foreground : string
    method hilitep : bool
    method is_shaped : bool
    method iter : (Gwml.wob_desc -> unit) -> unit
    method last_hook : Gwml.wob_event -> unit
    method mask : Xtypes.eventMask list
    method min_height : int
    method min_width : int
    method refresh : unit
    method resized : unit
    method reverse : bool
    method send : Gwml.wob_event -> unit
    method set_actions : Gwml.bindings -> unit
    method set_background : string -> unit
    method set_backimage : Gwml.image -> unit
    method set_backpixmap : Xtypes.pixmap -> unit
    method set_borderpixel : string -> unit
    method set_borderwidth : int -> unit
    method set_cursor : Gwml.cursor_desc -> unit
    method set_extensible_height : int -> unit
    method set_extensible_width : int -> unit
    method set_font : string -> unit
    method set_foreground : string -> unit
    method set_hilite : bool -> unit
    method set_hilite_background : string -> unit
    method set_hilite_backimage : Gwml.image -> unit
    method set_hilite_foreground : string -> unit
    method set_mask : Xtypes.eventMask list -> unit
    method set_min_height : int -> unit
    method set_min_width : int -> unit
    method set_shaped : bool -> unit
    method set_wob : Gwml.wob -> unit
    method unhilite : unit
    method update_bg : unit
    method update_fg : unit
    method wob : Gwml.wob
    method wob_hooks : Gwml.hook list
    method xevents : Xtypes.xevent -> unit
  end
val make : Gwml.wob -> Gwml.wob_desc -> Gwml.wob
val setenv : Gwml.wob -> 'a Wobenv.var -> 'a -> unit
val getenv : Gwml.wob -> 'a Wobenv.var -> 'a
val remenv : Gwml.wob -> 'a Wobenv.var -> unit
val sgetenv : Gwml.wob -> 'a Wobenv.var -> 'a -> 'a
val desc : #wob_base -> Gwml.wob_desc
val restore_clients : unit -> unit
val exit_gwml : 'a -> 'b
val restart_cmd : string array ref
val restart : 'a -> unit
val after_event : (unit -> unit) -> unit
val after_events : (unit -> unit) -> unit
val remove_expose : Xtypes.xevent -> unit