open Xtypes
open WX_types

let root = new WX_root.from_display "" 0
let top = new WX_wmtop.t root []
let label = new WX_button.with_label top#container "Hello World" []

let _ =
  top#container_add label#contained;
  label#set_action (fun _ -> exit 0);
  top#configure [Bindings [Key(XK.xk_q, anyModifier), (fun _ -> exit 0)]];
  top#show;
  loop () 
