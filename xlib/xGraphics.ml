(***********************************************************************)
(*                                                                     *)
(*                             Xlib                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* Module [Graphics]: machine-independent graphics primitives *)

open Xtypes
open Xlib

type update_style =
    ImmediateDraw
  | FlushAll
  | FlushClipped

let update_style = ref ImmediateDraw
let updates = ref []
  
exception Graphic_failure of string
        (* Raised by the functions below when they encounter an error. *)

(*** Initializations *)

type status =
  { mouse_x : int;              (* X coordinate of the mouse *)
    mouse_y : int;              (* Y coordinate of the mouse *)
    button : bool;              (* true if a mouse button is pressed *)
    mutable keypressed : bool;          (* true if a key has been pressed *)
    mutable key : char }                (* the character for the key pressed *)
        (* To report events. *)

type event =
  Button_down                 (* A mouse button is pressed *)
| Button_up                   (* A mouse button is released *)
| Key_pressed                 (* A key is pressed *)
| Mouse_motion                (* The mouse is moved *)
| Poll                        (* Don't wait; return immediately *)
        (* To specify events to wait for. *)
  
type view = {
    display : display;
    screen : screen;
    white : pixel;
    black : pixel;
    root : window;
    gc : gc;    
    window : window;
    mutable store : pixmap;
    cmap : colormap;
    mutable size_x : int;
    mutable size_y : int;
    mutable point_x : int;
    mutable point_y : int;
    mutable color : int;
    mutable pixel : pixel;
    mutable font : font;
    mutable descent : int;
    mutable ascent : int;
    mutable qf : queryFontRep;
    colors : (int,pixel) Hashtbl.t;
    pixels : (pixel,  int) Hashtbl.t;    
    depth : int;
    mutable s_in_events : (event * status) list; (* FIFO *)
    mutable s_out_events : (event * status) list;
    mutable s_keys : char list
  }
  
(*** Mouse and keyboard events *)

type events = {
    mutable button_down : bool;
    mutable button_up : bool;
    mutable key_pressed : bool;
    mutable poll : bool;
    mutable mouse_motion : bool;
  }  
  
let view = ref None
let do_handle_events = ref false

let clear v =
  X.changeGC v.display v.gc [GCforeground v.white];
  (match !update_style with
   ImmediateDraw -> clearWindow v.display v.window 
  | FlushAll -> ()
  | FlushClipped -> updates := [0,0,v.size_x,v.size_y]);
  X.polyFillRectangle v.display v.store v.gc [0,0,v.size_x,v.size_y];
  X.changeGC v.display v.gc [GCforeground v.pixel]        

let buttons = button1Mask + button2Mask + button3Mask

let window_handler v ev =
  match ev.ev_event with
  | ExposeEvent _ ->
      X.copyArea v.display v.gc v.store 0 0 v.window 0 0 v.size_x v.size_y
  | ConfigureNotifyEvent e ->
      let new_x = e.Xconfigure.width in
      let new_y = e.Xconfigure.height in
      if new_x <> v.size_x || new_y <> v.size_y then
        let old_store = v.store in
        let old_x = v.size_x in
        let old_y = v.size_y in
        v.store <- X.createPixmap v.display v.root new_x new_y v.depth;
        v.size_x <- new_x;
        v.size_y <- new_y;
        clear v;
        let dx = min new_x old_x in
        let dy = min new_y old_y in
        X.copyArea v.display v.gc old_store 0 (min 0 (old_y - dy))
        v.store 0 (new_y-dy) dx dy;
        X.freePixmap v.display old_store;
        X.clearArea v.display v.window 0 0 0 0 true
  | KeyPressEvent e when e.Xkey.event = v.window ->
      let (s,s1,s2) = KeyBind.lookupString v.display ev.ev_event in
      v.s_in_events <- (Key_pressed, { 
          mouse_x = e.Xkey.x_event;
          mouse_y = v.size_y - e.Xkey.y_event;
          button =  (e.Xkey.state land buttons) <> 0;
          keypressed = false;
          key = ' ';
        }) :: v.s_in_events;
      if String.length s = 1 then
        let c = s.[0] in
        v.s_keys <- v.s_keys@[c];
  | ButtonPressEvent e  when e.Xbutton.event = v.window -> 
      v.s_in_events <- (Button_down, { 
          mouse_x = e.Xbutton.x_event;
          mouse_y = v.size_y - e.Xbutton.y_event;
          button = true;
          keypressed = false;
          key = ' ';
        }) :: v.s_in_events;
  | ButtonReleaseEvent e when e.Xbutton.event = v.window -> 
      v.s_in_events <- (Button_up,{ 
          mouse_x = e.Xbutton.x_event;
          mouse_y = v.size_y - e.Xbutton.y_event;
          button = false;
          keypressed = false;
          key = ' ';
        })  :: v.s_in_events;
  | MotionNotifyEvent e when e.Xmotion.event = v.window -> 
      v.s_in_events <- (Mouse_motion,{ 
          mouse_x = e.Xmotion.x_event;
          mouse_y = v.size_y - e.Xmotion.y_event;
          button = (e.Xmotion.state land button1Mask) <> 0;
          keypressed = false;
          key = ' ';
        }) :: v.s_in_events
  | _ -> ()
      
let getview () = 
  match !view with None -> failwith "Graphic screen not opened" | Some v -> v

let default_screen_width = 600
let default_screen_height = 450
let border_width = 2
let window_name = "Caml graphics"
let icon_name = "Caml graphics"
let default_event_mask = [ExposureMask; KeyPressMask ;StructureNotifyMask;
    ButtonPressMask; ButtonReleaseMask; OwnerGrabButtonMask]
let default_font = "fixed"
let size_queue = 256

let clear_graph () = let v = getview () in clear v

let open_graph_display display width height =
  try
    let width = if width = 0 then default_screen_width else width in
    let height = if height = 0 then default_screen_height else height in
    let root = defaultRoot display in
    let screen = defaultScreen display in
    let white = defaultWhite display in
    let black = defaultBlack display in
    let cmap = defaultColormap display in
    let font = X.openFont display default_font in
    let depth = defaultDepth display in
    let gc = X.createGC display root [GCbackground white; 
        GCforeground black;
        GCfont font] in
    let window = createSimpleWindow display root 0 0 
        width height border_width
        [
        CWEventMask default_event_mask;
        CWBackPixel white
      ]
    in
    X.mapWindow display window;
    let store = X.createPixmap display root 
        width height depth in
    Icccm.setWM_NAME display window window_name;
    Icccm.setWM_ICON_NAME display window window_name;
    let qf = X.queryFont display font in
    let v = {
      display = display;
      screen = screen;
      white = white;
      black = black;
      root = root;
      gc = gc;
      window = window;
      store = store;
      font = font;
      qf = qf;
      descent = qf.qf_info.font_descent;
      ascent = qf.qf_info.font_ascent;
      size_x = width;
      size_y = height;
      point_x = 0;
      point_y = 0;
      color = 0;
      pixel = black;
      colors = Hashtbl.create 13;
      cmap = cmap;
      pixels = Hashtbl.create 13;
      s_out_events = [];
      s_in_events = [];
      s_keys = [];
      depth = depth;
      } in
    view := Some v;
    let d = Eloop.add_display display (fun _ -> ()) in
    Eloop.add_window d window (window_handler v);
    clear_graph ()
  with e -> raise e; raise (Graphic_failure "")

let open_graph name width height = 
  try
    let display = openDisplay name in
    open_graph_display display width height
  with e -> raise e;raise (Graphic_failure "")

let close_graph () =
  let v = getview () in
  closeDisplay v.display;
  view := None

let size_x () = (getview ()).size_x
let size_y () = (getview ()).size_y

type color = int

let rgb r g b = (r lsl 16) + (g lsl 8) + b

let black   = 0x000000
and white = 0xFFFFFF
and red     = 0xFF0000
and green   = 0x00FF00
and blue    = 0x0000FF
and yellow  = 0xFFFF00
and cyan    = 0x00FFFF
and magenta = 0xFF00FF

let background = white
and foreground = black

let pixel_rgb v color =
  try
    Hashtbl.find v.colors color
  with
    Not_found ->
      let pixel = 
        try
          (X.allocColor v.display v.cmap 
              (((color lsr 16) land 0xff) * 0x101) 
            (((color lsr 8) land 0xff) * 0x101) 
            ((color land 0xff) * 0x101)).ac_pixel
        with _ -> v.black
      in
      Hashtbl.add v.colors color pixel;
      Hashtbl.add v.pixels pixel color;
      pixel

let set_color color = 
  let v = getview () in
  if color <> v.color then
    let pixel = pixel_rgb v color in
    X.changeGC v.display v.gc [GCforeground pixel];
    v.pixel <- pixel;
    v.color <- color

let plot x y =
  let v = getview () in
  (match !update_style with
    ImmediateDraw -> X.polyPoint v.display v.window v.gc Origin [x,v.size_y - y]
  | FlushAll -> ()
  | FlushClipped -> updates := (x,v.size_y - y,1,1) :: !updates);
  X.polyPoint v.display v.store v.gc Origin [x,v.size_y - y]

let point_color x y =
  let v = getview () in
  let image = Zpixmap.getImage v.display v.store x (v.size_y - y) 1 1 0 in
  let pixel = Zpixmap.getPixel image 0 0 in
  try
    Hashtbl.find v.pixels pixel
  with
    Not_found ->
      let qc = X.queryColors v.display v.cmap [pixel] in
      let c = qc.(0) in
      let color = rgb (c.red / 0x101) (c.green / 0x101) (c.blue / 0x101) in
      Hashtbl.add v.pixels pixel color;
      color

let moveto x y = 
  let v = getview () in
  v.point_x <- x;
  v.point_y <- y

let current_point () =
  let v = getview () in
  v.point_x, v.point_y

let lineto x y =
  let v = getview () in
  let py = v.size_y - v.point_y in
  let px = v.point_x in
  let rx = x in
  let ry = v.size_y - y in
  v.point_x <- x;
  v.point_y <- y;
  (match !update_style with
    ImmediateDraw -> drawSegment v.display v.window v.gc px py rx ry
  | FlushAll -> ()
  | FlushClipped ->
      updates := (px,py,rx-px+1,ry-py+1) :: !updates);
  drawSegment v.display v.store v.gc px py rx ry

let draw_arc x y rx ry a1 a2 =
  let v = getview () in
  let xx = x - rx in
  let yy = v.size_y - y - ry in
  let r1 = rx * 2 in
  let r2 = ry * 2 in
  let aa = a1 * 64 in
  let bb = (a2 - a1) * 64 in
  (match !update_style with
    ImmediateDraw -> drawArc v.display v.window v.gc xx yy r1 r2 aa bb
  | FlushAll -> ()
  | FlushClipped ->
      updates := (xx,yy,r1,r2) :: !updates);
  drawArc v.display v.store v.gc xx yy r1 r2 aa bb

let draw_ellipse x y rx ry = draw_arc x y rx ry 0 360
let draw_circle x y r = draw_arc x y r r 0 360


let set_line_width w =
  let v = getview () in
  X.changeGC v.display v.gc [GCline_width w]

let draw_string s =
  let v = getview () in
  let xx = v.point_x in
  let yy = v.size_y - v.point_y - v.descent + 1 in
  let dx = Xtext.width v.qf s in
  (match !update_style with
    ImmediateDraw ->  drawString v.display v.window v.gc xx yy s
  | FlushAll -> ()
  | FlushClipped -> 
      let dy = v.ascent + v.descent in
      updates := (xx,yy - v.ascent, dx, dy) :: !updates);
  drawString v.display v.store v.gc xx yy s;
  v.point_x <- v.point_x + dx

let draw_char c =
  let s = String.make 1 c in
  draw_string s

let set_font name =
  let v = getview () in
  let font = try Xsync.openFont v.display name 
    with _ -> raise (Graphic_failure "font")
  in
  if font <> v.font then
    begin
      X.closeFont v.display v.font;
      v.font <- font;
      v.qf <- X.queryFont v.display font;
      v.ascent <- v.qf.qf_info.font_ascent;
      v.descent <- v.qf.qf_info.font_descent;
      X.changeGC v.display v.gc [GCfont font]
    end

let set_text_size _ = ()

let text_size s =
  let v = getview () in
  let ci = Xtext.extents v.qf s in
  ci.char_width, ci.char_ascent + ci.char_descent


let fill_arc x y rx ry a1 a2 =
  let v = getview () in
  let xx = x - rx in
  let yy = v.size_y - y - ry in
  let r1 = rx * 2 in
  let r2 = ry * 2 in
  let aa = a1 * 64 in
  let bb = (a2 - a1) * 64 in
  (match !update_style with
    ImmediateDraw -> X.polyFillArc v.display v.window v.gc [xx,yy,r1,r2,aa,bb]
  | FlushAll -> ()
  | FlushClipped -> updates := (xx,yy,r1,r2) :: !updates);
  X.polyFillArc v.display v.store v.gc [xx, yy,r1,r2,aa,bb]

let fill_ellipse x y rx ry = fill_arc x y rx ry 0 360
let fill_circle x y r = fill_arc x y r r 0 360

let fill_rect x y dx dy =
  let v = getview () in
  let yy = v.size_y - y - dy in
  (match !update_style with
    ImmediateDraw -> X.polyFillRectangle v.display v.window v.gc [x,yy,dx,dy]
  | FlushAll -> ()
  | FlushClipped -> updates := (x,yy,dx,dy) :: !updates);
  X.polyFillRectangle v.display v.store v.gc [x,yy,dx,dy]

let fill_poly tab =
  let v = getview () in
  let points = Array.map (fun (x,y) -> x, v.size_y - y) tab in
  let list = Array.to_list points in
  (match !update_style with
    ImmediateDraw -> X.fillPoly v.display v.window v.gc Complex Origin list
  | FlushAll -> ()
  | FlushClipped ->
      let n = Array.length points in
      if n > 0 then
      let (x,y) = points.(0) in
      let x1 = ref x in
      let x2 = ref x in
      let y1 = ref y in
      let y2 = ref y in
      for i = 1 to n - 1 do
        let (x,y) = points.(i) in
        if x < !x1 then x1 := x else
        if x > !x2 then x2 := x;
        if y < !y1 then y1 := y else
        if y > !y2 then y2 := y;
      done;
      updates := (!x1,!y2,!x2- !x1+1, !y2- !y2+1) :: !updates;
      );
  X.fillPoly v.display v.store v.gc Complex Origin list

type image = {
    w : int;
    h : int;
    im : pixmap;
    mutable mask : pixmap;
  }

let transp = -1

let create_image w h = 
  let v = getview () in
  { w = w;
    h = h;
    im = X.createPixmap v.display v.window w h v.depth;
    mask = noPixmap;
  }
  
let blit_image im x y = 
  let v = getview () in
  X.copyArea v.display v.gc v.store x (v.size_y - y - im.h + 1) im.im 0 0 im.w im.h

let draw_image im x y =
  let v = getview () in
  let y = v.size_y - y - im.h + 1 in
  if im.mask <> noPixmap then
    X.changeGC v.display v.gc 
      [GCclip_mask im.mask; GCclip_y_origin y; GCclip_x_origin x];
  X.copyArea v.display v.gc im.im 0 0 v.store x y im.w im.h;
  (match !update_style with
    ImmediateDraw -> X.copyArea v.display v.gc im.im 0 0 v.window x y im.w im.h
  | FlushAll -> ()
  | FlushClipped ->
      updates := (x,y,im.w,im.h)::!updates);
  if im.mask <> noPixmap then
    X.changeGC v.display v.gc [GCclip_mask noPixmap]

let make_image m = 
  let v = getview () in
  let h = Array.length m in
  if Array.length m = 0 then create_image 0 0 else
  let w = Array.length m.(0) in
  for i = 0 to h - 1 do
    if Array.length m.(i) <> w then
      raise (Graphic_failure "make_image: lines of different lengths")
  done;
  let has_transp = ref false in
  let zpix = Zpixmap.create v.display w h v.depth in
  for i = 0 to h - 1 do
    let line = m.(i) in
    for j = 0 to w - 1 do
      let color = line.(j) in
      let pixel = 
        if color = transp then (has_transp := true; v.white) else
          pixel_rgb v color
      in
      Zpixmap.setPixel zpix j i pixel
    done
  done;
  let im = create_image w h in
  let gc = X.createGC v.display im.im 
      [GCforeground (id_to_pixel 1); GCbackground (id_to_pixel 0)] in 
  Zpixmap.putImage v.display im.im v.gc 0 0 zpix;
  X.freeGC v.display gc;
  if !has_transp then
    begin
      let width = (w + 7)/8 in
      let bitmap = String.make (width * h) '\000' in
      for i = 0 to h - 1 do
        let line = m.(i) in
        for j = 0 to w - 1 do
          let color = line.(j) in
          if color <> transp then 
            bitmap.[i * width + j / 8 ] <- Char.chr
              ((Char.code bitmap.[i * width + j / 8 ])
              lor (1 lsl (j land 7)));
        done
      done;
      let (_,_,_,_,pix) = Xpm.createBitmapFromData v.display v.window 
          (w,h,0,0,bitmap) in
      im.mask <- pix;
    end;
  im

let dump_image _ = failwith "dump_image not implemented"

let get_image x y w h =
  let image = create_image w h in
  blit_image image x y;
  image

let next_event mask =
  let v = getview () in
  if mask.mouse_motion then
    (let _ = selectInput v.display v.window
          (PointerMotionMask:: OwnerGrabButtonMask :: default_event_mask) in ());
  let rec iter wait =
    if mask.key_pressed && v.s_keys <> [] && v.s_in_events = [] &&
      v.s_out_events = [] then mask.poll <- true;
    let wait = wait && not mask.poll in
    let _ = Eloop.handle_events wait in
    (match v.s_out_events with
        [] -> v.s_out_events <- List.rev v.s_in_events;
          v.s_in_events <- [] | _ -> ());
    match v.s_out_events with
      [] ->
        if mask.poll then
          let qp = X.queryPointer v.display v.window in
          { mouse_x = qp.qp_win_x;
            mouse_y = (v.size_y - qp.qp_win_y);
            button = (qp.qp_modifiers land button1Mask) <> 0;
            keypressed = false;
            key = ' '; }
        else
          iter true
    | (ev,s) :: tail -> 
        v.s_out_events <- tail; 
        match ev with
        | Mouse_motion when mask.mouse_motion -> s
        | Button_up when mask.button_up -> s
        | Button_down when mask.button_down -> s
        | _ -> if mask.poll then s else iter false
  in
  let s = iter false in
  if mask.mouse_motion then
    (let _ = selectInput v.display v.window default_event_mask in ());
  match v.s_keys with
    [] -> s
  | key :: tail -> 
      if mask.key_pressed then v.s_keys <- tail;
      s.keypressed <- true;
      s.key <- key;
      s

let  wait_next_event events =
  let mask = {
      button_down = false;
      button_up = false;
      key_pressed = false;
      poll = false;
      mouse_motion = false;
    } in
  let rec iter list =
    match list with
      [] -> ()
    | head :: tail -> 
        begin
          match head with
            Button_down -> mask.button_down <- true
          | Button_up -> mask.button_up <- true
          | Key_pressed -> mask.key_pressed <- true
          | Mouse_motion -> mask.mouse_motion <- true
          | Poll -> mask.poll <- true            
        end;
        iter tail
  in
  iter events;
  next_event mask

let mask_poll =
  {
    button_down = false;
    button_up = false;
    key_pressed = false;
    poll = true;
    mouse_motion = false;
  }
  
let mouse_pos () =
  let e = next_event mask_poll in (e.mouse_x, e.mouse_y)
  
let mask_button_down =
  {
    button_down = true;
    button_up = false;
    key_pressed = false;
    poll = true;
    mouse_motion = false;
  }
  
let button_down () =
  let e = next_event mask_button_down in e.button

let mask_read_key =
  {
    button_down = false;
    button_up = false;
    key_pressed = true;
    poll = false;
    mouse_motion = false;
  }
  
let read_key () =
  mask_read_key.poll <- false;
  let e = next_event mask_read_key in e.key
  
let key_pressed () =
  let e = next_event mask_poll in e.keypressed

let sound vfreq vdur =
  let v = getview () in
  X.changeKeyboardControl v.display [KBBellPitch vfreq; KBBellDuration vdur];
  X.bell v.display 0;
  X.changeKeyboardControl v.display [KBBellPitch (-1); KBBellDuration (-1)];
  ()

(* Update style *)  
  
let set_update_style r = 
  update_style := r;
  let v = getview () in
  X.copyArea v.display v.gc v.store 0 0 v.window 0 0 v.size_x v.size_y;
  updates := []

let update () =
  let v = getview () in
  match !update_style with
    ImmediateDraw -> ()
  | FlushAll ->
      X.copyArea v.display v.gc v.store 0 0 v.window 0 0 v.size_x v.size_y
  | FlushClipped ->
      List.iter (fun (x,y,dx,dy) ->
        X.copyArea v.display v.gc v.store x y v.window x y dx dy
        ) !updates;
      updates := []

(* PPM files *)
      (* Should use a lexer to speed-up. *)      
let rec getint f = 
  try
    let c = input_char f in
    match c with
      'P' | '#' -> let _ = input_line f in
        getint f
    | ' ' | '\n'-> getint f
    | _ -> let a = String.make 3 ' ' in 
      a.[0] <- c;
      let c2 = input_char f in
      match c2 with
        ' ' | '\n' -> a.[2] <- a.[0];
          a.[0] <- '0';
          a.[1] <- '0';
          int_of_string a
      | _ -> a.[1] <- c2;
          let c3 = input_char f in
          match c3 with
            ' ' | '\n' ->
              a.[2] <- a.[1];
              a.[1] <- a.[0];
              a.[0] <- '0';
              int_of_string a
          | _ -> a.[2] <- c3; 
              int_of_string a

  with
  | End_of_file -> close_in f;
      
      Pervasives.exit 0;;


(* conversion avec transparence *)
let create_image_from_ppm fichier =
  let f=open_in fichier in
  let d  = getint f 
  and  e = getint f 
  and  g = getint f in
  let image = Array.create_matrix e d (rgb 0 0 0) in 
  let c0 = input_byte f in
  for j = 0 to (e-1) do
    for i = 0 to (d-1) do
      let c1 = input_byte f
      and c2 = input_byte f
      and c3 = input_byte f in
      let col = rgb c1 c2 c3 in
      if col = (rgb 255 255 255) then image.(j).(i) <- transp
      else
        image.(j).(i) <- col;
    done
  done;
  close_in f;
  make_image image

(* XPM files *)
  
let create_image_from_xpm_data data =
  let v = getview () in
  let (dx,dy,depth,pixmap,mask) = Xpm.createPixmapFromData 
      v.display v.root v.cmap v.depth data
  in
  { w = dx;
    h = dy;
    im = pixmap;
    mask = mask
  }
  
let create_image_from_xpm file =
  create_image_from_xpm_data (Xpm.readPixmapDataFromFile file)
