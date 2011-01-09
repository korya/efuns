val cwx : int -> Xtypes.configureWindow
val cwy : int -> Xtypes.configureWindow
val cwwidth : int -> Xtypes.configureWindow
val cwheight : int -> Xtypes.configureWindow
val cwborderwidth : int -> Xtypes.configureWindow
val cwsibling : Xtypes.window -> Xtypes.configureWindow
val cwstackmode : Xtypes.stackMode -> Xtypes.configureWindow
val appendsome : ('a -> 'b) -> 'a option -> 'b list -> 'b list
module E :
  sig
    type t =
      Xtypes.Xconfigurerequest.t =
      { stack_mode: Xtypes.stackMode option;
        parent: Xtypes.window;
        window: Xtypes.window;
        sibling: Xtypes.window option;
        x: Xtypes.coord option;
        y: Xtypes.coord option;
        width: Xtypes.size option;
        height: Xtypes.size option;
        border_width: int option }
  end
val handle_ConfigureRequest : Gwml.wob -> E.t -> unit
val set_shape : Gwml.client_desc -> Gwml.wob -> unit
val c_is_shaped : Gwml.client_desc -> bool
val handle_shapeNotify :
  Gwml.client_desc -> Gwml.wob -> Shape.XshapeNotify.t -> unit
val client_xevents : Gwml.client_desc -> Gwml.wob -> Xtypes.xevent -> unit
val parent_events : 'a -> Gwml.wob -> Xtypes.xevent -> unit
module F :
  sig
    type t =
      Xtypes.Xconfigure.t =
      { event: Xtypes.window;
        window: Xtypes.window;
        above_sibling: Xtypes.window;
        x: Xtypes.coord;
        y: Xtypes.coord;
        width: Xtypes.size;
        height: Xtypes.size;
        border_width: int;
        override_redirect: bool }
  end
val sendSyntheticConf : Gwml.client_desc -> Gwml.wob -> unit
val allow_withdrawn_state : bool Options.option_record
val window_borderwidth : int Options.option_record
val window_borderpixel : string Options.option_record
class client :
  Gwml.client_desc ->
  object
    inherit Wob.wob_base
  end
val last_hook : 'a -> 'b -> unit
val make : Gwml.client_desc -> client
val create : Gwml.wob -> Xtypes.window -> bool -> Gwml.client_desc
