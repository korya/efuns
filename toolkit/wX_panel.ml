(***********************************************************************)
(*                                                                     *)
(*                            WXlib                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


open Xtypes
open WX_types

  (* On doit aussi pouvoir faire une classe WX_container contenant
  un certain nombre de widgets, et offrant les methodes hide, show, realize,
  etc ... *)

class t (sens : bar_desc) parent (adj : WX_adjust.t) attributes =
  object (self)
  
  
  val mutable first = (new WX_dummy.t parent [])#contained
  val mutable second = (new WX_dummy.t parent [])#contained
  val mutable separator = (new WX_dummy.t parent [])#contained
  val mutable sens = sens
  val adj = adj
  val mutable step = 1      
      
    inherit WX_object.t parent attributes as super
  
  initializer 
    adj#add_subject self#update_childs
  
  method sens = sens
  method realize = 
    super#realize;
    first#realize;
    second#realize;
    separator#realize
  
  method set_step s = 
    step <- s; 
    let g = w.w_geometry in
    adj#set_inc (match sens with Vertical -> g.height/s | _ -> g.width/s);
  
  method show = 
    super#show;
    first#show;
    second#show;
    separator#show
  
  method destroy = 
    super#destroy;
    first#destroy;
    second#destroy;
    separator#destroy
  
  method size_request = 
    let sz = szhints in
    if (* sz.comp_timestamp = s.s_timestamp *) not w.w_size_modified  || sz.comp_timestamp = s.s_timestamp then sz else
    let sz1 = first#size_request in
    sz.comp_timestamp <- s.s_timestamp;
    let sz2 = separator#size_request in
    let sz3 = second#size_request in
    let (width, height) =
      match sens with
        Vertical ->
          let height = 
            sz1.requested_height + sz2.requested_height + 
              sz3.requested_height +
              2 * (sz1.border_width + sz2.border_width + sz3.border_width) +
              4 * w.w_ipad_y in
          let width = 2 * w.w_ipad_x +
              (max (max
                  (sz1.requested_width + 2 * sz1.border_width)
                (sz2.requested_width + 2 * sz2.border_width))
              (sz3.requested_width + 2 * sz3.border_width))
          in
          width, height
      | Horizontal ->
          let width = 
            sz1.requested_width + sz2.requested_width + sz3.requested_width + 
              2 * (sz1.border_width + sz2.border_width + sz3.border_width) +
              4 * w.w_ipad_x 
          in
          let height = 2 * w.w_ipad_y +
              (max (max
                  (sz1.requested_height + 2 * sz1.border_width)
                (sz2.requested_height + 2 * sz2.border_width))
              (sz3.requested_height + 2 * sz3.border_width))
          in
          width, height 
    in
    sz.requested_width <- max sz.min_width width;
    sz.requested_height <- max sz.min_height height;
    sz
  
  method size_allocate x y dx dy = 
    let g = w.w_geometry in
    let modified = w.w_size_modified || not (g.width = dx && g.height = dy) in
    super#size_allocate x y dx dy;
    if modified then
      begin
        adj#set_inc (match sens with Vertical -> dy/step | _ -> dx/step);
        self#update_childs ()
      end
  
  method set_first (o : contained) = 
    first#hide;
    first <- o;
    if w.w_shown then o#show;
    self#wait_resize
  
  method set_separator (o : contained) = 
    separator#hide;
    separator <- o;
    if w.w_shown then o#show;
    self#wait_resize
  
  method set_second (o : contained) = 
    second#hide;
    second <- o;
    if w.w_shown then o#show;
    self#wait_resize
  
  method move_separator dx dy = 
    let g = w.w_geometry in
    let offset,total = match sens with
        Vertical -> dy, g.height - 2 * w.w_ipad_y
      | Horizontal -> dx, g.width - 2 * w.w_ipad_x
    in
    let cur = adj#get_pos total in
    adj#set_pos (cur + offset) total
  
  method update_childs () =
    let sz1 = first#size_request in
    let sz2 = separator#size_request in
    let sz3 = second#size_request in    
    let g = w.w_geometry in
    let width = g.width - 2 * w.w_ipad_x in
    let height = g.height - 2 * w.w_ipad_y in
    match sens with
      Vertical ->
        let x1 = w.w_ipad_x + sz1.border_width in
        let x2 = w.w_ipad_x + sz2.border_width in
        let x3 = w.w_ipad_x + sz3.border_width in
        let w1 = width - 2 * (w.w_ipad_x + sz1.border_width) in
        let w2 = width - 2 * (w.w_ipad_x + sz2.border_width) in
        let w3 = width - 2 * (w.w_ipad_x + sz3.border_width) in
        let pos = adj#get_pos height in
        let h2 = sz2.requested_height in
        let y1 = w.w_ipad_y in
        let y2 = pos - h2 / 2 - sz2.border_width in
        let h1 = max 1 (y2 - y1 - w.w_ipad_y - 2 * sz1.border_width) in
        let y3 = y2 + h2 + 2 * sz2.border_width + w.w_ipad_y + 
            sz3.border_width in
        let h3 = height - y3 - w.w_ipad_y - 2 * sz3.border_width in
        first#size_allocate x1 y1 w1 h1;
        separator#size_allocate x2 y2 w2 h2;
        second#size_allocate x3 y3 w3 h3
    
    | Horizontal ->
        let y1 = w.w_ipad_y + sz1.border_width in
        let y2 = w.w_ipad_y + sz2.border_width in
        let y3 = w.w_ipad_y + sz3.border_width in
        let h1 = height - 2 * (w.w_ipad_y + sz1.border_width) in
        let h2 = height - 2 * (w.w_ipad_y + sz2.border_width) in
        let h3 = height - 2 * (w.w_ipad_y + sz3.border_width) in
        let pos = adj#get_pos width in
        let w2 = sz2.requested_height in
        let x1 = w.w_ipad_x in
        let x2 = pos - w2 / 2 - sz2.border_width in
        let w1 = min 1 (x2 - x1 - w.w_ipad_x - 2 * sz1.border_width) in
        let x3 = x2 + w2 + 2 * sz2.border_width + w.w_ipad_x + 
            sz3.border_width in
        let w3 = width - x3 - w.w_ipad_x - 2 * sz3.border_width in
        first#size_allocate y1 x1 h1 w1;
        separator#size_allocate y2 x2 h2 w2;
        second#size_allocate y3 x3 h3 w3
  
  method name = "panel"
end

class separator (t : t) adj attributes =
  object (self)

  val xc_hand2 = t#cursor_make (FontCursor XC.xc_hand2) true
  
  inherit WX_object.t t#container ((
      Cursor (FontCursor XC.xc_arrow)) :: 
    attributes) as super
  
  val sens = t#sens
  val mutable mini_win = noWindow
  
  initializer
    t#set_separator self#contained;
    self#configure [Bindings [ButtonPress, self#track_mouse]]
  
  method size_request =
    let sz = szhints in
    sz.requested_width <- 10;
    sz.requested_height <- 10;
    (match sens with
        Vertical -> sz.expand_y <- true
      | Horizontal -> sz.expand_x <- true);
    sz
  
  method refresh =
    if s.s_timestamp > w.w_refresh_timestamp && not (w.w_window == noWindow) 
    then    
      begin
        super#refresh;
        let fg = w.w_foreground.c_pixel in
        let bg = w.w_background.c_pixel in
        let g = w.w_geometry in
        let gc = GCCache.get2 s.s_gcs fg bg in
        let draw = Xlib.drawLine s.s_display w.w_window gc in
        let w = g.width in
        let h = g.height in
        begin
          match sens with
            Vertical -> 
              draw 0 (h/2 - 2 ) w (h/2 - 2);
              draw 0 (h/2 + 2 ) w (h/2 + 2);

          | Horizontal -> 
              draw (w/2-2) 0 (w/2-2) h;
              draw (w/2+2) 0 (w/2+2) h;
        end;
      end

  method track_mouse () =
    X.changeActivePointerGrab s.s_display 
      [PointerMotionHintMask; ButtonMotionMask; ButtonReleaseMask;
      ButtonPressMask; OwnerGrabButtonMask]
      xc_hand2.curs_id !Eloop.event_time;
    let fg = w.w_foreground.c_pixel in
    let bg = w.w_background.c_pixel in
    let pg = parent#geometry in
    let pwin = parent#window in
    let mgc = X.createGC s.s_display pwin 
        [GCfonction GXxor;
        GCforeground s.s_screen.scr_white_pixel;
        GCbackground s.s_screen.scr_black_pixel;
        GCsubwindow_mode IncludeInferiors] in
    let drawn = ref false in
    let line = Xlib.drawLine s.s_display pwin mgc in
    let total = match sens with
        Vertical -> pg.height | Horizontal -> pg.width
    in
    let value = ref (adj#get_pos total) in
    let draw () =
      drawn := not !drawn;
      match sens with
          Vertical -> line 0 !value pg.width !value
      | Horizontal -> line !value 0 !value pg.height
    in
    let motion () = 
      let qp = X.queryPointer s.s_display pwin in
      if !drawn then draw ();
      value := (match sens with
          Vertical -> qp.qp_win_y | Horizontal -> qp.qp_win_x);
      draw ()
    in
    try
      while true do
        let ev = Xlib.peekEvent s.s_display in
        match ev.ev_event with
        | ButtonReleaseEvent _ ->
            motion ();
            adj#set_pos !value total;
            raise Exit
        | ButtonPressEvent _
        | KeyPressEvent _ ->
            raise Exit
        | MotionNotifyEvent _ -> 
            motion ()
        | _ -> Xlib.putBackEvent s.s_display ev
      done
    with
      _ -> 
        if !drawn then draw ();
        X.ungrabPointer s.s_display currentTime;
        X.freeGC s.s_display mgc
        
  method name = "panel_separator"
end