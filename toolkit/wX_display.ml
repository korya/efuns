(***********************************************************************)
(*                                                                     *)
(*                             ____                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Xtypes
open WX_types

class from display = 
  object (self)
    val eloop = Eloop.add_display display (fun ev -> 
          match ev.ev_event with
          | SelectionRequestEvent e -> 
              Selection.handleSelectionRequest display e
          | SelectionClearEvent e ->
              Selection.handleSelectionClear display e
          | _ -> ()      
      )
    val display = display
    
    method display = display
    method eloop = eloop
    method close = 
      Eloop.remove_display eloop;
      Xlib.closeDisplay display
    method broken f =
      display.dpy_broken <- f
end


class t name =
  let display = Xlib.openDisplay name in
  from display