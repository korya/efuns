open WX_types

let root = new WX_root.from_display "" 0
let top = new WX_wmtop.t root []
let label = new WX_label.t top#container "Hello World" []

let _ =
  top#container_add label#contained;
  top#show;
  loop ()
