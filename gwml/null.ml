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

open Xtypes
open Gwml
open Wob


class null =
  object (self)
  inherit wob_base
  
  method first_hook e = 
    let w = self#wob in
    match e with
    | WobGetSize -> 
        w.w_geometry.width <- min_width;
        w.w_geometry.height <- min_height;
        w.w_geometry.x <- 1;
        w.w_geometry.y <- 1;
    | WobCreate ->
        self#create true;
        self#update_bg;
        X.mapWindow display w.w_window;
    | WobResize _ ->
        if w.w_window <> noWindow then
          let g = w.w_geometry in
          Xlib.moveResizeWindow display w.w_window g.x g.y 
            (max 1 g.width) (max 1 g.height);
          self#resized
    | WobDestroy ->
        if w.w_window <> noWindow then
          let s = w.w_screen in      
          Eloop.remove_window s.s_scheduler w.w_window;
          X.destroyWindow display w.w_window;
          w.w_window <- noWindow
    | WobRefresh ->
        X.clearArea display w.w_window 0 0 0 0 false
    | WobKeyPress (e,s,k) -> self#handle_key (e,s,k)
    | WobButtonPress e ->  self#handle_button e        
    | WobUpdateShape ->
        if is_shaped then begin
            let w = self#wob in
            let g = w.w_geometry in
            let module S = Shape in
            let module X = S.X in
            X.shapeRectangles display w.w_window S.Bounding 0 0
              [ 0, 0, g.width, g.height ] S.Set UnSorted;
            X.shapeRectangles display w.w_window S.Bounding 0 0
              [ 0, 0, g.width, g.height ] S.Substract UnSorted;
          end
    | _ -> () 
        
  method no_shape =
    is_shaped <- true;
    try
      let w = self#wob in
      if not (w.w_window == noWindow) then
        Wob.send_one w WobUpdateShape
    with _ -> ()
end

let make () = new null

let make_wob () = (new null :> wob_desc)

  
  