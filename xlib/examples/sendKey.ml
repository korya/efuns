open Sys
open Xtypes
open Xlib
open X

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

let string_to_modifier s =  
  let mask = ref 0 in
  for i = 0 to String.length s - 1 do
    mask := !mask lor (match s.[i] with
        'S' -> shiftMask
      | 'C' -> controlMask
      | 'A' -> anyModifier
      | 'M' -> mod1Mask
      | '1' -> mod1Mask
      | '2' -> mod2Mask
      | '3' -> mod3Mask
      | '4' -> mod4Mask
      | '5' -> mod5Mask
      | _ -> 0
    )
  done;
  if !mask land anyModifier <> 0 then anyModifier else !mask
  
let name_to_keysym = 
  ("Button1", XK.xk_Pointer_Button1) ::
  ("Button2", XK.xk_Pointer_Button2) ::
  ("Button3", XK.xk_Pointer_Button3) ::
  ("Button4", XK.xk_Pointer_Button4) ::
  ("Button5", XK.xk_Pointer_Button5) ::
  ("DblClick1", XK.xk_Pointer_DblClick1) ::
  ("DblClick2", XK.xk_Pointer_DblClick2) ::
  ("DblClick3", XK.xk_Pointer_DblClick3) ::
  ("DblClick4", XK.xk_Pointer_DblClick4) ::
  ("DoubleClick5", XK.xk_Pointer_DblClick5) ::
  ("DoubleClick1", XK.xk_Pointer_DblClick1) ::
  ("DoubleClick2", XK.xk_Pointer_DblClick2) ::
  ("DoubleClick3", XK.xk_Pointer_DblClick3) ::
  ("DoubleClick4", XK.xk_Pointer_DblClick4) ::
  ("DoubleClick5", XK.xk_Pointer_DblClick5) ::
  ("DeltaMove1", XK.xk_Pointer_Drag1) ::
  ("DeltaMove2", XK.xk_Pointer_Drag2) ::
  ("DeltaMove3", XK.xk_Pointer_Drag3) ::
  ("DeltaMove4", XK.xk_Pointer_Drag4) ::
  ("DeltaMove5", XK.xk_Pointer_Drag5) ::
  ("DMovee1", XK.xk_Pointer_Drag1) ::
  ("DMove2", XK.xk_Pointer_Drag2) ::
  ("DMove3", XK.xk_Pointer_Drag3) ::
  ("DMove4", XK.xk_Pointer_Drag4) ::
  ("DMove5", XK.xk_Pointer_Drag5) ::
  XK.name_to_keysym
  
  (* form: SC-Button1 *)
let string_to_key s =
  let key, mods = 
    try
      let index = String.index s '-' in
      let mods = String.sub s 0 index in
      let key = String.sub s (index+1) (String.length s - index - 1) in
      key, mods
    with _ -> s, ""
  in
  let key = List.assoc key name_to_keysym in
  let mods = string_to_modifier mods in    
  mods, key

let display = ref None
let window = ref None

let list_win d scr =
  let root = scr.scr_root in
  let rec iter w =
    let qt = X.queryTree d w in
    let name = try Icccm.getWM_NAME d w with _ -> "" in
    let classe = try match Icccm.getWM_CLASS d w with
          [c;s] -> c ^ "." ^ s
            | _ -> "." with _ -> "."
          in
    let full = classe ^ "." ^ name in
    if full <> ".." then begin
        Printf.printf "Name: %s" full; print_newline ();
      end;
    List.iter iter qt.qt_subwindows;
  in 
  iter root
  
let find_named_win d scr  reg =
  try
    let r = Str.regexp reg in
    let root = scr.scr_root in
    let win = ref root in
    let rec iter w =
      let qt = X.queryTree d w in
      let name = try Icccm.getWM_NAME d w with _ -> "" in
      let classe = try match Icccm.getWM_CLASS d w with
            [c;s] -> c ^ "." ^ s
          | _ -> "." with _ -> "."
      in
      let full = classe ^ "." ^ name in
      if full <> ".." then begin
          if Str.string_match r full 0 then
            begin win := w; raise Exit end;
        end;
      List.iter iter qt.qt_subwindows;
    in 
    (try iter root; raise Not_found  with Exit -> ());
    let qp = X.queryPointer d !win in  
    !win, qp
  with Not_found ->
      failwith (Printf.sprintf "Window \"%s\" not found" reg)
  
let find_property_win d scr prop =
  try
    let property = X.internAtom d prop false in
    let root = scr.scr_root in
    let p = X.getProperty d root false property XA.xa_window 0 4 in
    let s = p.gp_value in
    if String.length s <> 4 then raise Not_found;
    Printf.printf  "%d.%d.%d.%d" (Char.code s.[0]) (Char.code s.[1]) (Char.code s.[2]) (Char.code s.[3]);
    print_newline ();
    let win = (((Char.code s.[3] * 256 )+ Char.code s.[2]) * 256 + Char.code s.[1]) * 256 + Char.code s.[0] in
    let qp = X.queryPointer d win in  
    win, qp
  with Not_found ->
      failwith (Printf.sprintf "Window from \"%s\" not found" prop)
  | XError e ->
      failwith (Printf.sprintf "Error : %s" (xerror_to_string e))
      
let find_mouse_win d root =
  let rec iter win =
    let qp = X.queryPointer d win in  
    if qp.qp_win == noWindow then win, qp else
      iter qp.qp_win
  in
  iter root

let getDisplay () =
    match !display with
      None -> let d = Xlib.openDisplay "" in
        let scr = d.dpy_roots.(0) in
        display := Some(d,scr);
        d,scr
    | Some (d,scr) -> d,scr
  
let getWindow () =
  match !window with
    Some (d,scr,win,qp) -> d,scr,win,qp
  | None ->
      let d,scr = getDisplay () in
      let win, qp = find_mouse_win d scr.scr_root in
      window := Some (d,scr,win,qp);
      d,scr,win,qp
      
let sendString str =
  let d,scr,win,qp = getWindow () in
  let root = scr.scr_root in
  let c = qp.qp_win in
  for i = 0 to String.length str - 1 do
    let cc = keysym2string str.[i] in
    let m, k = name2keysym cc in
    let detail,m = KeyBind.keysymToKeycode d k in
    let ev = {
          Xkey.detail = detail;
          Xkey.time = currentTime;
          Xkey.root = scr.scr_root;
          Xkey.event = win;
          Xkey.child = noWindow;
          Xkey.x_event = qp.qp_win_x;
          Xkey.y_event = qp.qp_win_y;
          Xkey.x_root = qp.qp_root_x;
          Xkey.y_root = qp.qp_root_y;
          Xkey.state = m;
          Xkey.same_screen = true
      } in
    X.sendEvent d win true [] (KeyPressEvent ev);
    X.sendEvent d win true [] (KeyReleaseEvent ev)
  done

type event = Press | Release | Both
  
let sendKey kind str =
  let d,scr,win,qp = getWindow () in
  let root = scr.scr_root in
  let m, k = string_to_key str in
  let detail,m = KeyBind.keysymToKeycode d k in
  let ev = {
      Xkey.detail = detail;
      Xkey.time = currentTime;
      Xkey.root = scr.scr_root;
      Xkey.event = win;
      Xkey.child = noWindow;
      Xkey.x_event = qp.qp_win_x;
      Xkey.y_event = qp.qp_win_y;
      Xkey.x_root = qp.qp_root_x;
      Xkey.y_root = qp.qp_root_y;
      Xkey.state = m;
      Xkey.same_screen = true
    } in
  match kind with
    Both ->
      X.sendEvent d win true [] (KeyPressEvent ev);
      X.sendEvent d win true [] (KeyReleaseEvent ev)
  | Press ->
      X.sendEvent d win true [] (KeyPressEvent ev);
  | Release ->
      X.sendEvent d win true [] (KeyReleaseEvent ev)
  
let sendButton kind i =
  let d, scr, win, qp = getWindow () in
  let ev =  {
      Xbutton.detail = i;
      Xbutton.time = currentTime;
      Xbutton.root = scr.scr_root;
      Xbutton.event = win;
      Xbutton.child = noWindow;
      Xbutton.x_event = qp.qp_win_x;
      Xbutton.y_event = qp.qp_win_y;
      Xbutton.x_root = qp.qp_root_x;
      Xbutton.y_root = qp.qp_root_y;
      Xbutton.state = qp.qp_modifiers;
      Xbutton.same_screen = true
    } in
  match kind with
    Both ->
      X.sendEvent d win true [] (ButtonPressEvent ev);
      X.sendEvent d win true [] (ButtonReleaseEvent ev)
  | Press ->
      X.sendEvent d win true [] (ButtonPressEvent ev);
  | Release ->
      X.sendEvent d win true [] (ButtonReleaseEvent ev)
  
let sendClient s =
  try
  let d, scr, win, qp = getWindow () in
  let ev =  {
      Xclient.format = 1;
      Xclient.window = win;
      Xclient.datatype = XA.xa_string;
      Xclient.data = s;
      }
  in
  Xsync.sendEvent d win true [] (ClientMessageEvent ev)
  with
    XError e ->
      Printf.printf "Error: %s" (Xlib.xerror_to_string e);
      print_newline ()

      
let main () =
  let d = Unix.gettimeofday () in
  let ic = open_out (Printf.sprintf "/tmp/sendKey%f" d) in
  output_string ic "Bonjour";
  close_out ic;
  
  Arg.parse [
    "-display", Arg.String (fun s ->
        (match !display with  None -> ()
          | Some (d,_) -> Xlib.closeDisplay d);
        let d = Xlib.openDisplay s in
        let scr = d.dpy_roots.(0) in
        display := Some(d,scr)
    ), "<name>: set display name";
    
    "-name", Arg.String (fun s ->
        let d,scr = getDisplay () in
        let win, qp = find_named_win d scr s in
        window := Some (d,scr,win,qp)        
    ), "<regexp>: set window name";
    
    "-list", Arg.Unit (fun _ ->
        let d,scr = getDisplay () in
        list_win d scr
    ), ": list window names";
    "-string", Arg.String sendString, "<str>: send string <str> to client";
    "-key", Arg.String (sendKey Both), "<key>: send key <key> to client";
    "-button", Arg.Int (sendButton Both), "<button>: send click <button> to client";    
    "-keypress", Arg.String (sendKey Press), "<key>: send key <key> to client";
    "-buttonpress", Arg.Int (sendButton Press), "<button>: send click <button> to client";    
    "-keyrelease", Arg.String (sendKey Release), "<key>: send key <key> to client";
    "-buttonrelease", Arg.Int (sendButton Release), "<button>: send click <button> to client";
    "-client", Arg.String sendClient, "<string>: send client event";
    "-property", Arg.String (fun s ->
        let d,scr = getDisplay () in
        let win, qp = find_property_win d scr s in
        window := Some (d,scr,win,qp)        
    ), "<property>: send event to window in property";
    "-win", Arg.Int (fun w ->
        let d,scr = getDisplay () in
        let win = w in
        let qp = X.queryPointer d win in  
        window := Some (d,scr,win,qp);
    ), "<window>: destination window"
    ] (fun s ->
      sendString s) "sendKey: sends some events to some X client"
    
let _ = main ()