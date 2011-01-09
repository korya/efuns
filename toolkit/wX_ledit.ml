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

class orig parent string attributes =
  object (self)
    
    inherit WX_label.orig parent string 
      ((Relief ReliefSunken):: (Cursor (FontCursor XC.xc_xterm)) :: (MinWidth 100) :: attributes)
    as super
    
    val mutable subjects = []
    method add_subject f = subjects <- f :: subjects
    method update = List.iter (fun f -> try f () with _ -> ()) subjects
    initializer
      self#configure [Bindings [
          Key (XK.xk_Return, 0), (fun _ -> ());
          Key (XK.xk_BackSpace, 0), (fun _ ->
              if cursor_x > 0 then
                begin
                  cursor_x <- cursor_x - 1;
                  self#set_string (
                    let s = self#string in
                    let len = String.length s in
                    (String.sub s 0 cursor_x) ^ (
                      String.sub s (cursor_x+1) (len - 1 - cursor_x)));
                  self#update
                end
          );
          Key (XK.xk_Delete, 0), (fun _ ->
              if cursor_x < String.length string then
                begin
                  self#set_string (
                    let s = self#string in
                    let len = String.length s in
                    (String.sub s 0 cursor_x) ^ (
                      String.sub s (cursor_x+1) (len - 1 - cursor_x)));
                  self#update
                end
          );
          Key (XK.xk_Left, 0), (fun _ ->
              cursor_x <- max 0 (cursor_x - 1);
              self#wait_refresh true 0 0 0 0);
          Key (XK.xk_Right, 0), (fun _ ->
              cursor_x <- min (String.length string) (cursor_x + 1);
              self#wait_refresh true 0 0 0 0);
          Key (anyKey, anyModifier), (fun _ ->
              self#set_string (
                let s = self#string in
                let len = String.length s in
                (String.sub s 0 cursor_x) ^ !key_string ^ (
                  String.sub s cursor_x (len - cursor_x)));
              cursor_x <- cursor_x + (String.length !key_string);
              self#update);
          FocusIn ,(fun _ -> 
              display_cursor <- true; self#wait_refresh true 0 0 0 0);
          FocusOut ,(fun _ -> 
              display_cursor <- false; self#wait_refresh true 0 0 0 0);
        ]
      ];
      cursor_x <- String.length string
      
end

class t = orig
  