val broadcast : Gwml.wob array -> Gwml.wob_event -> unit
val set_shape : Gwml.wob -> Gwml.wob array -> unit
class bar :
  Gwml.bar_desc ->
  Gwml.wob_desc array ->
  object
    inherit Wob.wob_base
    method add_item : Gwml.wob_desc -> unit
    method insert_item : int -> Gwml.wob_desc -> unit
    method insert_items : int -> Gwml.wob_desc array -> unit
    method items : Gwml.wob_desc array
    method nitems : int
    method remove_item : int -> unit
  end
val make : Gwml.bar_desc -> Gwml.wob_desc array -> bar
