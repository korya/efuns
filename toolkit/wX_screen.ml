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

let rec screen_resize s =
  if not (s.s_wait_resize == []) then
    begin
      s.s_timestamp <- s.s_timestamp + 1;
      let list = s.s_wait_resize in
      s.s_wait_resize <- [];
      List.iter (fun o -> o#update_size) list;
      screen_resize s
    end

let rec iter_refresh list =
  match list with
    [] -> ()
  | o :: tail ->
      o#refresh;
      iter_refresh tail
    
let screen_update s =
  screen_resize s;
  if not (s.s_wait_refresh == []) then 
    begin
      s.s_timestamp <- s.s_timestamp + 1;
      let list = s.s_wait_refresh in
      s.s_wait_refresh <- [];
      iter_refresh list;
    end
    
  
class virtual t screen = 
    object (self)
  val s = screen
  
  method screen = s
  method display = s.s_display
  
  method font_make font_name safe =
    let font_name = String.lowercase font_name in
    let s = self#screen in
    let display = s.s_display in
    try
      Hashtbl.find s.s_fonts font_name
    with
      Not_found ->
        try
          let font = X.openFont display font_name in
          let qf = X.queryFont display font in
          let maxb = qf.qf_info.font_max_bounds in
          let font = { 
              font_name = font_name;
              font_id = font; font_info = qf;
              font_ascent = maxb.char_ascent;
              font_height = maxb.char_ascent + maxb.char_descent;
              font_width = maxb.char_width;
            } in
          Hashtbl.add s.s_fonts font_name font;
          font
        with
          _ when safe -> s.s_default_font
            
(* We should optimize the case where Visual = DirectColor, and color
  is specified as #rgb *)            
  method color_make color safe = (* safe: do not raise any exception *)
    self#global_color_make (Namedcolor (String.lowercase color)) safe
  
  method global_color_make c safe =
    try
      Hashtbl.find s.s_colors c
    with
      Not_found ->
        let scr = s.s_screen in
        let display = s.s_display in
        let cmap = scr.scr_default_colormap in
        try
          let color =
            match c with
              Namedcolor c ->
                if String.length c = 7 && c.[0] = '#' then
                  let r = hex c.[2] + 16 * hex c.[1] in let r = 255 * r in
                  let g = hex c.[4] + 16 * hex c.[3] in let g = 255 * g in
                  let b = hex c.[6] + 16 * hex c.[5] in let b = 255 * b in 
                  let ac = X.allocColor display cmap r g b in
                  { c_pixel = ac.ac_pixel;
                    c_red = ac.ac_color.red;
                    c_green = ac.ac_color.green;
                    c_blue = ac.ac_color.blue;
                  }
                else
                if String.length c = 13 && c.[0] = '#' then
                  let r = hex c.[2] + 16 * hex c.[1] in
                  let g = hex c.[6] + 16 * hex c.[5] in
                  let b = hex c.[10] + 16 * hex c.[9] in
                  let ac = X.allocColor display cmap r g b in
                  { c_pixel = ac.ac_pixel;
                    c_red = ac.ac_color.red;
                    c_green = ac.ac_color.green;
                    c_blue = ac.ac_color.blue;
                  }
                else
                let anc = X.allocNamedColor display cmap c in
                { c_pixel = anc.anc_pixel;
                  c_red = anc.anc_exact.red;
                  c_green = anc.anc_exact.green;
                  c_blue = anc.anc_exact.blue;
                }
            | RgbColor (r,g,b) ->
                let ac = X.allocColor display cmap r g b in
                { c_pixel = ac.ac_pixel;
                  c_red = ac.ac_color.red;
                  c_green = ac.ac_color.green;
                  c_blue = ac.ac_color.blue;
                }
          in
          Hashtbl.add s.s_colors c color;
          color
        with
          _ when safe -> s.s_default_color
  
  method  pixmap_make (name,pixmap) =
    let s = self#screen in
    try Hashtbl.find s.s_pixmaps name with Not_found ->
        let scr = s.s_screen in
        let display = s.s_display in
        let pixmap = 
          match pixmap with
            FromFile file ->
              let file = if Filename.is_relative file then
                  Utils.find_in_path !pixmap_path file else file
              in
              Xpm.createPixmapFromFile display scr.scr_root
                scr.scr_default_colormap scr.scr_root_depth file
          | FromFunction (pix,mask) -> 
              let gg = X.getGeometry display pix in
              gg.gg_width, gg.gg_height, gg.gg_depth, pix, mask
          | FromData data ->
              Xpm.createPixmapFromData display scr.scr_root
                scr.scr_default_colormap scr.scr_root_depth data
        in
        Hashtbl.add s.s_pixmaps name pixmap;
        pixmap
  
  method cursor_make name safe =
    let s = self#screen in
    try
      Hashtbl.find s.s_cursors name
    with
      Not_found -> try
          let screen = s.s_screen in
          let display = s.s_display in
          let cursor = 
            match name with
              FontCursor name ->
                Xlib.createFontCursor display name 
            | NoCursor -> Xtypes.noCursor
            | BitmapCursor (bitmap,mask) ->
                let bitmap = Utils.find_in_path !pixmap_path bitmap in
                let mask = Utils.find_in_path !pixmap_path mask in
                let (w,h,hot_x,hot_y,pix) = 
                  Xpm.createBitmapFromFile display screen.scr_root bitmap in
                try
                  let (ww,hh,_,_,mask) = 
                    Xpm.createBitmapFromFile display screen.scr_root mask in
                  try
                    if w = ww &&  h = hh then
                      X.createCursor display pix mask 0 0 0 0xFFFF 0xFFFF 0xFFFF
                        hot_x hot_y else raise Not_found 
                  with e -> X.destroyWindow display mask; raise e
                with _ -> X.destroyWindow display pix;
                    Xlib.createFontCursor display XC.xc_x_cursor 
          in
          let cursor = {
              curs_desc = name;
              curs_id = cursor;
            } in
          Hashtbl.add s.s_cursors name cursor;
          cursor
        with _ when safe -> noCursor
  
  method click_type =
    try
      let s = self#screen in
      let display = s.s_display in
      let eloop = s.s_eloop in      
      let e1 = match Eloop.last_event eloop  with
          ButtonPressEvent e -> e
        | _ -> raise Exit
      in
      let e2 = s.s_last_click in
      if e2.Xbutton.detail = e1.Xbutton.detail &&
        e2.Xbutton.event = e1.Xbutton.event &&
        (real_time e1.Xbutton.time) -. (real_time e2.Xbutton.time)
        < !double_click_delay
      then Double else
      let qp = X.queryPointer display e1.Xbutton.root in
      let old_x = e1.Xbutton.x_root in
      let old_y = e1.Xbutton.y_root in
      if abs(qp.qp_root_x - old_x) >= !delta_move_size ||
        abs(qp.qp_root_y - old_y) >= !delta_move_size then Simple
      else
        DeltaMove
    with
      _ -> Other
  
  
  
  method getShadow bg_color = (* Taken from Fvwm code *)
    let r = bg_color.c_red mod 0xffff in
    let g = bg_color.c_green mod 0xffff in
    let b = bg_color.c_blue mod 0xffff in
    
    let r = r lsr 1 in
    let g = g lsr 1 in
    let b = b lsr 1 in
    
    try
      self#global_color_make (RgbColor(r,g,b)) false
    with
      _ -> bg_color
  
  method getHilite bg_color = 
    let white = self#color_make "white" false in
    
    let r = max (white.c_red/5) bg_color.c_red in
    let g = max (white.c_green/5) bg_color.c_green in
    let b = max (white.c_blue/5) bg_color.c_blue in
    
    let r = min white.c_red ((r*140)/100) in
    let g = min white.c_green ((g*140)/100) in
    let b = min white.c_blue ((b*140)/100) in
    
    try
      self#global_color_make (RgbColor(r,g,b)) false
    with _ -> bg_color

    (* The update method will update widget sizes and refresh on demand *)
  method update = screen_update s
end
