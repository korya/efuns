(***********************************************************************)
(*                                                                     *)
(*                            GwML                                     *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Xtypes
open Gwml
  
let keysym2string c =
  let code = Char.code c in
  if code < 32 then "C-" ^ (String.make 1 (Char.chr (64+code))) else
  match c with
  | ' ' -> "space"
  | '\n' -> "Linefeed"
  | '\r' -> "Return"
  | '\t' -> "Tab"
  | '!' -> "S-1"
  | '"' -> "S-apostrophe"
  | '#' -> "S-3"
  | '$' -> "S-4"
  | '%' -> "S-5"
  | '&' -> "S-7"
  | '\'' -> "apostrophe"
  | '(' -> "S-9"
  | ')' -> "S-0"
  | '*' -> "S-8"
  | '+' -> "S-equal"
  | ',' -> "comma"
  | '-' -> "minus"
  | '.' -> "period"
  | '/' -> "slash"
  | ':' -> "S-semicolon"
  | ';' -> "semicolon"
  | '<' -> "less"
  | '=' -> "equal"
  | '>' -> "S-period"
  | '?' -> "S-slash"
  | '@' -> "S-2"
  | '[' -> "bracketleft"
  | '\\' -> "backslash"
  | ']' -> "bracketright"
  | '^' -> "S-6"
  | '_' -> "S-minus"
  | '`' -> "grave"
  | '{' -> "S-bracketleft"
  | '|' -> "S-backslash"
  | '}' -> "S-bracketright"
  | '~' -> "S-grave"
  | _ -> String.make 1 c    

let name2keysym s =
  let m,s = if String.length s > 2 &&  String.sub s 0 2 = "S-" then 
      shiftMask, String.sub s 2 (String.length s - 2) else 0,s
  in
  let m,s = if String.length s > 2 && String.sub s 0 2 = "C-" then 
      m lor controlMask, String.sub s 2 (String.length s - 2) else m,s
  in
  let m,s = if String.length s > 2 && String.sub s 0 2 = "M-" then 
      m lor mod1Mask, String.sub s 2 (String.length s - 2) else m,s
  in
  try
    m, List.assoc s XK.name_to_keysym
  with Not_found ->
      Printf.printf  "Failed to find keysym for <%s>" s; print_newline ();
      raise Not_found
  
let sendKeyPress wob str =
  let w = wob#wob in
  let c = wob#client in
  let tw = w.w_top in
  let g = tw.w_geometry in
  let sw = tw.w_parent in
  let s = w.w_screen in
  let scr = s.s_scr in
  try
  for i = 0 to String.length str - 1 do
    let cc = keysym2string str.[i] in
      let m, k = name2keysym cc in
      let detail, m = KeyBind.keysymToKeycode display k in
    X.sendEvent display c.c_window false [] (
      KeyPressEvent {
        Xkey.detail = detail;
        Xkey.time = currentTime;
        Xkey.root = scr.scr_root;
        Xkey.event = c.c_window;
        Xkey.child = c.c_window;
        Xkey.x_event = 1;
        Xkey.y_event = 1;
        Xkey.x_root = g.x + 1;
        Xkey.y_root = g.y + 1;
        Xkey.state = m;
        Xkey.same_screen = true
      }
    )
  done
  with e ->
      Printf.printf "E %s" (Utils.printexn e);
      print_newline ();
      raise e
