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

let default_font = ref "fixed"
let default_foreground = ref "white"

class ledit string =
  object (self)
  inherit Label.label string as label
  initializer 
    label#set_mask (ButtonPressMask :: KeyPressMask :: label#mask);
    label#set_min_width 50;
  
  val mutable locked = false
  val mutable edit = false
  
  method edit_set_string = ()
    
  method inverse =
    let fg = self#foreground in
    let bg = self#background in
    self#set_foreground bg;
    self#set_background fg;
  
  method  first_hook e =
    let w = self#wob in
    let s = w.w_screen in
    match e with
      WobLeave _ -> 
        if edit then
          ( self#hilite;
            label#set_cursor (FontCursor XC.xc_cross);          
            edit <- false;
            Wob.send w.w_top (WobSetInputFocus true))
    | WobKeyPress (e,s,k) -> 
        if edit then
          begin
            locked <- true;        
            if k = XK.xk_BackSpace then
              label#set_string (
                let s = label#string in
                let len = String.length s in
                String.sub s 0 (len - 1))
            else if k = XK.xk_Return then
              ( edit <- false;
                label#set_cursor NoCursor;          
                self#hilite;
                self#edit_set_string;
                Wob.send w.w_top (WobSetInputFocus true))
            else if k = XK.xk_Delete then
              label#set_string "" 
            else
              label#set_string (label#string ^ s)
          end
    
    | WobButtonPress _ -> 
        locked <- true;
        edit <- true;
        label#set_cursor (FontCursor XC.xc_xterm);
        self#hilite;
        X.setInputFocus display self#wob.w_window RevertToPointerRoot 
          !Eloop.event_time
    | _ -> label#first_hook e
        
  method set_string s = if not locked then label#set_string s
      
end

let make string = new ledit string
let desc wob = (wob :> wob_desc)  
