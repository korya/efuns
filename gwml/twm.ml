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

open Options
open Xlib
open Xtypes
open Gwml
open Stdconfig
open Twm_t
open Wob
  
let load file =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  try
    let tree = Twm_p.twmrc Twm_l.twmrc lexbuf in
    close_in ic;
    tree
  with
    e -> close_in ic; raise e

let cursors =
  [
    "X_cursor",		XC.xc_x_cursor;
    "arrow",		XC.xc_arrow;
    "based_arrow_down",	XC.xc_based_arrow_down;
    "based_arrow_up",	XC.xc_based_arrow_up;
    "boat",		XC.xc_boat;
    "bogosity",		XC.xc_bogosity;
    "bottom_left_corner",	XC.xc_bottom_left_corner;
    "bottom_right_corner",	XC.xc_bottom_right_corner;
    "bottom_side",		XC.xc_bottom_side;
    "bottom_tee",		XC.xc_bottom_tee;
    "box_spiral",		XC.xc_box_spiral;
    "center_ptr",		XC.xc_center_ptr;
    "circle",		XC.xc_circle;
    "clock",		XC.xc_clock;
    "coffee_mug",		XC.xc_coffee_mug;
    "cross",		XC.xc_cross;
    "cross_reverse",	XC.xc_cross_reverse;
    "crosshair",		XC.xc_crosshair;
    "diamond_cross",	XC.xc_diamond_cross;
    "dot",			XC.xc_dot;
    "dotbox",		XC.xc_dotbox;
    "double_arrow",	XC.xc_double_arrow;
    "draft_large",		XC.xc_draft_large;
    "draft_small",		XC.xc_draft_small;
    "draped_box",		XC.xc_draped_box;
    "exchange",		XC.xc_exchange;
    "fleur",		XC.xc_fleur;
    "gobbler",		XC.xc_gobbler;
    "gumby",		XC.xc_gumby;
    "hand1",		XC.xc_hand1;
    "hand2",		XC.xc_hand2;
    "heart",		XC.xc_heart;
    "icon",		XC.xc_icon;
    "iron_cross",		XC.xc_iron_cross;
    "left_ptr",		XC.xc_left_ptr;
    "left_side",		XC.xc_left_side;
    "left_tee",		XC.xc_left_tee;
    "leftbutton",		XC.xc_leftbutton;
    "ll_angle",		XC.xc_ll_angle;
    "lr_angle",		XC.xc_lr_angle;
    "man",			XC.xc_man;
    "middlebutton",	XC.xc_middlebutton;
    "mouse",		XC.xc_mouse;
    "pencil",		XC.xc_pencil;
    "pirate",		XC.xc_pirate;
    "plus",		XC.xc_plus;
    "question_arrow",	XC.xc_question_arrow;
    "right_ptr",		XC.xc_right_ptr;
    "right_side",		XC.xc_right_side;
    "right_tee",		XC.xc_right_tee;
    "rightbutton",		XC.xc_rightbutton;
    "rtl_logo",		XC.xc_rtl_logo;
    "sailboat",		XC.xc_sailboat;
    "sb_down_arrow",	XC.xc_sb_down_arrow;
    "sb_h_double_arrow",	XC.xc_sb_h_double_arrow;
    "sb_left_arrow",	XC.xc_sb_left_arrow;
    "sb_right_arrow",	XC.xc_sb_right_arrow;
    "sb_up_arrow",		XC.xc_sb_up_arrow;
    "sb_v_double_arrow",	XC.xc_sb_v_double_arrow;
    "shuttle",		XC.xc_shuttle;
    "sizing",		XC.xc_sizing;
    "spider",		XC.xc_spider;
    "spraycan",		XC.xc_spraycan;
    "star",		XC.xc_star;
    "target",		XC.xc_target;
    "tcross",		XC.xc_tcross;
    "top_left_arrow",	XC.xc_top_left_arrow;
    "top_left_corner",	XC.xc_top_left_corner;
    "top_right_corner",	XC.xc_top_right_corner;
    "top_side",		XC.xc_top_side;
    "top_tee",		XC.xc_top_tee;
    "trek",		XC.xc_trek;
    "ul_angle",		XC.xc_ul_angle;
    "umbrella",		XC.xc_umbrella;
    "ur_angle",		XC.xc_ur_angle;
    "watch",		XC.xc_watch;
    "xterm",		XC.xc_xterm;
  ]

let newFontCursor name = FontCursor (List.assoc name cursors)
      
let twm_titlebar_width = ref 17
let twm_titlebar_border = ref 1
let twm_pixmap_offset = ref 2
  
let twm_bordercolor = Client.window_borderpixel
let twm_borderwidth = Client.window_borderwidth
let twm_TitleBackground = title_background
let twm_TitleForeground = title_foreground

let twm_MenuTitleForeground = menu_title_foreground
let twm_MenuTitleBackground = menu_title_background  
  
let twm_FrameCursor = ref (newFontCursor "top_left_arrow")
let twm_TitleCursor = ref (newFontCursor "top_left_arrow")
let twm_IconCursor = ref (newFontCursor "top_left_arrow")
let twm_IconMgrCursor = ref (newFontCursor "top_left_arrow")
let twm_MoveCursor = ref (newFontCursor "fleur")
let twm_MenuCursor = ref (newFontCursor "sb_left_arrow")
let twm_ButtonCursor = ref (newFontCursor "hand2")
let twm_WaitCursor = ref (newFontCursor "watch")
let twm_SelectCursor = ref (newFontCursor "dot")
let twm_DestroyCursor = ref (newFontCursor "pirate")

let twm_WindowKeys =  ref []
let twm_TitleKeys =  ref []
let twm_IconKeys =  ref []
let twm_RootKeys =  ref []
let twm_FrameKeys =  ref []
let twm_IconMgrKeys =  ref []
  
let createResizePixmap s =
  let scr = s.s_scr in
  let h = !twm_titlebar_width - (!twm_titlebar_border+ !twm_pixmap_offset) * 2
    in
  let h = if h < 1 then 1 else h in
  let pix = X.createPixmap display scr.scr_root h  h 1 in
  let gc = X.createGC display pix [GCforeground (id_to_pixel 0)] in
  fillRectangle display pix gc 0 0 h h;
  let lw = h / 16 in
  let lw = if lw = 1 then 0 else lw in
  X.changeGC display gc [GCforeground (id_to_pixel 1);
    GCline_style LineSolid; GCcap_style CapButt; GCjoin_style JoinMiter;
    GCline_width lw];
  (* draw the resize button *)
  let w = (h * 2) / 3 in
  X.polyLine display pix gc Origin  [w,0; w,w; 0,w];
  let w = w / 2 in
  X.polyLine display pix gc Origin  [w,0; w,w; 0,w];
  X.freeGC display gc;
  pix

let createDotPixmap s =
  let scr = s.s_scr in
  let h = !twm_titlebar_width - !twm_titlebar_border * 2 in
  let h = (h * 3) / 4 in
  let h = if h < 1 then 1 else h in
  let h = if h land 1 = 1 then h-1 else h in
  let pix = X.createPixmap display scr.scr_root h  h 1 in
  let gc = X.createGC display pix
    [GCforeground (id_to_pixel 0);
      GCline_style LineSolid; GCcap_style CapRound; GCjoin_style JoinRound;
      GCline_width (h/2)] in
      (* draw the dot button *)
  fillRectangle display pix gc 0 0 h h;
  X.changeGC display gc [GCforeground (id_to_pixel 1)];  
  drawSegment display pix gc (h/2) (h/2) (h/2) (h/2);
  X.freeGC display gc;
  pix

let createMenuIcon s =
  let height = 20 in
  let scr = s.s_scr in
  let h = height in
  let w = h * 7 / 8 in
  let h = if (h < 1) then 1 else h in
  let w = if (w < 1) then 1 else w in
  let pix = X.createPixmap display scr.scr_root w h 1 in
  let gc = X.createGC  display pix [GCforeground (id_to_pixel 0)] in
  fillRectangle display pix gc 0 0 w h;
  setForeground display gc (id_to_pixel 1);
  let ix = 1 in
  let iy = 1 in
  let ih = h - iy * 2 in
  let iw = w - ix * 2 in
  let off = ih / 8 in
  let mh = ih - off in
  let mw = iw - off in
  let bw = mh / 16 in
  let bw = if (bw == 0 && mw > 2) then 1 else bw in
  let tw = mw - bw * 2 in
  let th = mh - bw * 2 in
  fillRectangle display pix gc ix iy mw mh;
  fillRectangle display pix gc (ix + iw - mw) (iy + ih - mh) mw mh;
  setForeground display gc (id_to_pixel 0);
  fillRectangle display pix gc (ix+bw) (iy+bw) tw th;
  setForeground display gc (id_to_pixel 1);
  let lw = tw / 2 in
  let lw = if ((tw land 1) lxor (lw land 1)) <> 0 then lw + 1 else lw in
  let lx = ix + bw + (tw - lw) / 2 in
  let lh = th / 2 - bw in
  let lh = if ((lh land 1) lxor ((th - bw) land 1)) <>0 then lh +1 else lh in
  let ly = iy + bw + (th - bw - lh) / 2 in
  let lines = 3 in
  let lines = if (lh land 1) <> 0 && lh < 6 then lines-1 else lines in
  let dly = lh / (lines - 1) in
  let rec iter lines ly =
    if lines > 0 then
      let lines = lines - 1 in
      fillRectangle display pix gc lx ly lw bw;
      iter lines (ly + dly)
  in
  iter lines ly;
  X.freeGC display gc;
  pix  

let dot_pixmap = FromFunction ("twm_dot_pixmap", fun sw ->
      createDotPixmap sw.w_screen, noPixmap)
  
  
let resize_pixmap = FromFunction ("twm_resize_pixmap", fun sw ->
      createResizePixmap sw.w_screen, noPixmap)
  
let menu_pixmap = FromFunction ("twm_menu_pixmap", fun sw ->
      createMenuIcon sw.w_screen, noPixmap)
  
let twm_menu s () = try Hashtbl.find menus_table ("twmrc:"^s) with _ -> []
let twm_function s w = 
  try (Hashtbl.find funs_table ("twmrc:"^s)) w with _ -> ()

      let delta_stop w =
  if Wob.click w.w_screen <> DeltaMove then raise Exit
(*  let (old_x,old_y) =
    match !Eloop.last_event with
      KeyPressEvent e -> e.Xkey.x_root, e.Xkey.y_root
    | KeyReleaseEvent e -> e.Xkey.x_root, e.Xkey.y_root
    | ButtonPressEvent e -> e.Xbutton.x_root, e.Xbutton.y_root
    | ButtonReleaseEvent e -> e.Xbutton.x_root, e.Xbutton.y_root
    | MotionNotifyEvent e -> e.Xmotion.x_root, e.Xmotion.y_root
    | EnterNotifyEvent e -> e.Xcrossing.x_root, e.Xcrossing.y_root
    | LeaveNotifyEvent e -> e.Xcrossing.x_root, e.Xcrossing.y_root
    | _ -> raise Exit
  in
  let qp = X.queryPointer display w.w_screen.s_scr.scr_root in
  if abs(qp.qp_root_x - old_x) >= !delta_move_size ||
    abs(qp.qp_root_y - old_y) >= !delta_move_size then () else
    raise Exit
*)
    (* If the current wob is associated with a client, find the associated client, else interact with user to select a window. *)


        
let twm_action f w =
  match f with
  | F_AUTORAISE -> ()
  | F_BACKICONMGR -> ()
  | F_BEEP -> X.bell display 3
  | F_BOTTOMZOOM -> ()
  | F_CIRCLEDOWN -> ()
  | F_CIRCLEUP -> ()
  | F_CUTFILE -> ()
  | F_DEICONIFY -> ()
  | F_DELETE -> Wob.send w.w_top WobDeleteWindow
  | F_DELTASTOP -> delta_stop w
  | F_DESTROY -> X.killClient display (window_to_id 
          ((User.client_top w).w_oo#client.c_window))
  | F_DOWNICONMGR -> ()
  | F_FOCUS -> ()
  | F_FORCEMOVE -> User.move (User.client_top w) true
  | F_FORWICONMGR -> ()
  | F_FULLZOOM -> ()
  | F_HBZOOM -> ()
  | F_HIDEICONMGR -> ()
  | F_HORIZOOM -> ()
  | F_HTZOOM -> ()
  | F_HZOOM -> ()
  | F_ICONIFY -> Wob.send (User.client_top w) (WobIconifyRequest true)
  | F_IDENTIFY -> Wob.send (User.client_top w) (WobIconifyRequest true)
  | F_LEFTICONMGR -> ()
  | F_LEFTZOOM -> ()
  | F_LOWER -> Wob.send  (User.client_top w) WobLowerWindow
  | F_MOVE -> User.move (User.client_top w) true
  | F_NEXTICONMGR -> ()
  | F_NOP -> ()
  | F_PREVICONMGR -> ()
  | F_QUIT -> Wob.exit_gwml ()
  | F_RAISE -> Wob.send (User.client_top w) WobRaiseWindow
  | F_RAISELOWER -> 
      X.configureWindow display (User.client_top w).w_window [CWStackMode Opposite]
  | F_REFRESH -> ()
  | F_RESIZE -> resize (User.client_top w) false
  | F_RESTART -> restart ()
  | F_RIGHTICONMGR -> ()
  | F_RIGHTZOOM -> ()
  | F_SAVEYOURSELF -> ()
  | F_SHOWICONMGR -> ()
  | F_SORTICONMGR -> ()
  | F_TITLE -> ()
  | F_TOPZOOM -> ()
  | F_TWMRC -> restart ()
  | F_UNFOCUS -> ()
  | F_UPICONMGR -> ()
  | F_VERSION -> ()
  | F_VLZOOM -> ()
  | F_VRZOOM -> ()
  | F_WINREFRESH -> ()
  | F_ZOOM -> ()
  | F_SCROLLUP -> 
      let rw = w.w_top.w_parent in
      let rg = rw.w_geometry in      
      !virtual_manager#move w 0 (-rg.height)
  | F_SCROLLDOWN -> 
      let rw = w.w_top.w_parent in
      let rg = rw.w_geometry in      
      !virtual_manager#move w 0 rg.height
  | F_SCROLLLEFT -> 
      let rw = w.w_top.w_parent in
      let rg = rw.w_geometry in      
      !virtual_manager#move w (-rg.width) 0
  | F_SCROLLRIGHT -> 
      let rw = w.w_top.w_parent in
      let rg = rw.w_geometry in      
      !virtual_manager#move w (rg.width) 0
  | F_SCROLLHOME -> 
      let rw = w.w_top.w_parent in
      let rg = rw.w_geometry in      
      let dx, dy = !virtual_manager#current_position w in
      !virtual_manager#move w (-dx) (-dy)

let twm_saction f s w =
  match f with
  | F_COLORMAP -> ()
  | F_CUT -> ()
  | F_EXEC -> commandw s ()
  | F_FILE -> ()
  | F_FUNCTION -> twm_function s w
  | F_MENU -> let _ = popup_menu w false (twm_menu s ()) in ()
  | F_PRIORITY -> ()
  | F_SOURCE -> ()
  | F_WARPRING -> ()
  | F_WARPTO -> ()
  | F_WARPTOICONMGR -> ()
  | F_WARPTOSCREEN -> ()

let twm_func list w =
  let rec iter list =
    match list with
      [] -> ()
    | (Action f) :: tail -> twm_action f w; iter tail
    | (ActionString (f,s)) :: tail -> twm_saction f s w; iter tail
  in
  iter list

let keysym string = 
  try List.assoc string XK.name_to_keysym with
    _ -> Printf.printf "Unkown key binding <%s>" string; print_newline ();
      raise Not_found
      
let install_binding where key action grab =
  let key = 
    key, (match action with
        Action f -> twm_action f
      | ActionString (f,s) -> twm_saction f s), grab
  in
  List.iter (fun context ->
      match context with
      | C_WINDOW -> twm_WindowKeys := key :: !twm_WindowKeys
      | C_TITLE -> twm_TitleKeys := key :: !twm_TitleKeys
      | C_ICON -> twm_IconKeys := key :: !twm_IconKeys
      | C_ROOT -> twm_RootKeys := key :: !twm_RootKeys
        | C_FRAME -> twm_FrameKeys := key :: !twm_FrameKeys
        | C_ICONMGR -> twm_IconMgrKeys := key :: !twm_IconMgrKeys
        | C_ALL -> 
            twm_WindowKeys := key :: !twm_WindowKeys;
            twm_TitleKeys := key :: !twm_TitleKeys;
            twm_IconKeys := key :: !twm_IconKeys;
            twm_RootKeys := key :: !twm_RootKeys;
            twm_FrameKeys := key :: !twm_FrameKeys;
            twm_IconMgrKeys := key :: !twm_IconMgrKeys;
        | C_NAME win -> ()        
    ) where
      
let use tree =
  List.iter (fun op ->
      match op with
        Error -> ()
      | NoArg keyword -> 
          begin
            match keyword with
              F_AUTORELATIVERESIZE -> ()
            | F_CLIENTBORDERWIDTH -> ()
            | F_DECORATETRANSIENTS -> ()
            | F_DONTMOVEOFF -> confine_move =:= true
            | F_FORCEICONS -> ()
            | F_INTERPOLATEMENUCOLORS -> ()
            | F_NOBACKINGSTORE -> ()
            | F_NOCASESENSITIVE -> ()
            | F_NODEFAULTS -> ()
            | F_NOGRABSERVER -> grab_server =:= false
            | F_NOICONMANAGERS -> ()
            | F_NOMENUSHADOWS -> ()
            | F_NORAISEONWARP -> ()
            | F_NORAISEONRESIZE -> ()
            | F_NORAISEONMOVE -> ()
            | F_NORAISEONDEICONIFY -> ()
            | F_NOSAVEUNDERS -> ()
            | F_NOTITLEFOCUS -> ()
            | F_NOVERSION -> ()
            | F_OPAQUEMOVE -> opaque_move =:= true
            | F_RANDOMPLACEMENT -> ()
            | F_RESTARTPREVIOUSSTATE -> ()
            | F_SHOWICONMANAGER -> ()
            | F_SORTICONMANAGER -> ()
            | F_WARPUNMAPPED -> ()
            | F_SHOWVIRTUALNAMES -> ()               
          end
      | StringArg (skeyword,string) -> 
          begin
            match skeyword with
              F_ICONDIRECTORY -> ()
            | F_ICONFONT -> icon_font =:= string
            | F_ICONMANAGERFONT -> iconMgr_font =:= string
            | F_MAXWINDOWSIZE -> ()
            | F_MENUFONT -> menu_font =:= string
            | F_RESIZEFONT -> resize_font =:= string
            | F_TITLEFONT -> window_font =:= string
            | F_UNKNOWNICON -> ()
            | F_USEPPOSITION -> ()          
            | F_VIRTUALDESKTOP -> ()
          end
      | NumberArg (nkeyword,int) -> 
          begin
            match nkeyword with
              F_BORDERWIDTH -> ()
            | F_BUTTONINDENT -> ()
            | F_CONSTRAINEDMOVETIME -> ()
            | F_FRAMEPADDING -> ()
            | F_ICONBORDERWIDTH -> icon_borderwidth =:= int
            | F_MOVEDELTA -> ()
            | F_NPRIORITY -> ()
            | F_TITLEBUTTONBORDERWIDTH -> ()
            | F_TITLEPADDING -> ()
            | F_XORVALUE -> ()              
          end
      | AddIconRegion (string, dir1, dir2, i1, i2) -> ()
      | IconMgrGeometry (string, option) -> ()
      | ZoomCount int -> ()
      | Pixmap_list strings -> ()
      | Cursor_list cursors -> 
          List.iter (fun cursor ->
              let where, cursor = match cursor with
                  NewFontCursor (where, name) -> where,newFontCursor name
                | NewBitmapCursor (where, cursor, mask) -> 
                    where, BitmapCursor (cursor,mask)
              in
              match where with
                FrameCursor -> twm_FrameCursor := cursor
              | TitleCursor -> twm_TitleCursor := cursor
              | IconCursor -> twm_IconCursor := cursor
              | IconMgrCursor -> twm_IconMgrCursor := cursor
              | ButtonCursor -> twm_ButtonCursor := cursor
              | MoveCursor -> twm_MoveCursor := cursor
              | ResizeCursor -> User.twm_ResizeCursor := cursor
              | WaitCursor -> twm_WaitCursor := cursor
              | MenuCursor -> twm_MenuCursor := cursor
              | SelectCursor -> twm_SelectCursor := cursor
              | DestroyCursor -> twm_DestroyCursor := cursor              
          ) cursors
      | Sticky win_list -> ()
      | IconifyByUnmapping win_list -> ()
      | IconifyByUnmappingAll -> ()
      | TitleButton (string, action, bool) -> ()
      | ButtonMenu (int, string) -> 
          install_binding [C_ROOT]  (Gwml.Button(int, 0)) 
          (ActionString (F_MENU, string)) false
      | ButtonAction (int, action) -> 
          install_binding [C_ROOT]  (Gwml.Button(int, 0)) 
          action false
      | Key (string, mods, where, action) -> 
          (try          
              install_binding where (Gwml.Key(keysym string, mods)) action 
                (mods <> 0)
            with _ -> ())
      | Button (int, mods, where, action) ->
          (try
              install_binding where (Gwml.Button(int, mods)) action (mods <> 0)
            with _ -> ())  
      | DontIconify win_list -> ()
      | IconManagerNoShow win_list -> ()
      | IconManagerNoShowAll -> ()
      | IconManagerShow win_list -> ()
      | IconMgrs iconms -> ()
      | NoTitleHighlight win_list -> ()
      | NoTitleHighlightAll -> ()
      | NoHighlight win_list -> ()
      | NoStackMode win_list -> ()
      | NoTitlebar win_list -> ()
      | NoHighlightAll -> ()
      | NoStackModeAll -> ()
      | NoTitlebarAll -> ()
      | MakeTitle win_list -> ()
      | StartIconified win_list -> ()
      | AutoRaise win_list -> ()
      | RootMenu (string, str_opt1, str_opt2, menus) -> 
          let menu = List.map (fun (name,action,_,_) ->
                match action with
                | ActionString (F_MENU,s) -> name, [
                      ItemPixmap (menu_pixmap, false)
                    ],Menu (twm_menu s)
                | Action F_TITLE -> name, [
                      ItemForeground (fun _ -> !!twm_MenuTitleForeground);
                      ItemBackground (fun _ -> !!twm_MenuTitleBackground)], 
                    Function (fun w -> ());
                | Action f -> name, [], Function (twm_action f)
                | ActionString (f,s) -> name, [],Function (twm_saction f s)
            ) menus in
          Hashtbl.add menus_table ("twmrc:"^string) menu
      | RootFunction (string, fonction) -> 
          Hashtbl.add funs_table  ("twmrc:"^string)  (twm_func fonction)
      | IconNames couples -> ()
      | ColorList colors -> 
          List.iter (fun (clkeyword, name, specials) -> 
              match clkeyword with                
                F_BORDERCOLOR -> twm_bordercolor =:= name
              | F_BORDERTILEBACKGROUND -> ()
              | F_BORDERTILEFOREGROUND -> ()
              | F_ICONBACKGROUND -> icon_background =:= name
              | F_ICONBORDERCOLOR -> ()
              | F_ICONFOREGROUND -> icon_foreground =:= name
              | F_ICONMANAGERFOREGROUND -> iconMgr_foreground =:= name
              | F_ICONMANAGERBACKGROUND -> iconMgr_background =:= name
              | F_ICONMANAGERHIGHLIGHT -> ()
              | F_TITLEFOREGROUND -> twm_TitleForeground =:= name
              | F_TITLEBACKGROUND -> twm_TitleBackground =:= name
              | F_DEFAULTBACKGROUND -> Wob.default_background =:= name
              | F_DEFAULTFOREGROUND -> Wob.default_foreground =:= name
              | F_MENUBACKGROUND -> menu_background =:= name
              | F_MENUFOREGROUND -> menu_foreground =:= name
              | F_MENUSHADOWCOLOR -> ()
              | F_MENUTITLEBACKGROUND -> twm_MenuTitleBackground =:= name
              | F_MENUTITLEFOREGROUND -> twm_MenuTitleForeground =:= name
              | F_POINTERBACKGROUND -> ()
              | F_POINTERFOREGROUND -> ()
              | F_PANNERBACKGROUND -> panner_background =:= name
              | F_PANNERFOREGROUND -> panner_foreground =:= name
              | F_VIRTUALFOREGROUND -> ()
              | F_VIRTUALBACKGROUND -> ()
          ) colors
      | GrayscaleList colors -> ()
      | Monochrome colors -> ()
      | DefaultFunction action -> ()
      | WindowFunction action -> ()
      | WarpCursorList win_list -> ()
      | WarpCursorAll -> ()
      | WindowRingList win_list -> ()
      | SaveColorList save_colors -> ()
      | SqueezeTitleList squeezes -> ()
      | SqueezeTitleAll -> ()
      | DontSqueezeTitleList win_list -> ()
      | DontSqueezeTitleAll -> ()
      
  ) tree
  
open Stddeco  
  
let twm_hook w e =
  match e with
    WobInit -> 
      w#set_borderpixel !!twm_bordercolor;
      w#set_borderwidth !!twm_borderwidth;
      w#set_cursor !twm_FrameCursor;
  | _ -> ()
      
let twm_window sw c =
  let label = 
    let label = 
      (* if !editable_title then 
      (new c_ledit c :> Label.label) else
    *)
      Label.make c.c_name in
    label#add_hook (name_update c label);
    label#set_min_height (!twm_titlebar_width - !twm_titlebar_border * 2);
    label#set_font !!window_font;
    label#set_background !!twm_TitleBackground;
    label#set_foreground !!twm_TitleForeground;
    label#set_extensible_width 2;
    label
  in
  
  let left_pixmap = Pixmap.make sw dot_pixmap in
  left_pixmap#add_hook (fun e ->
      match e with
        WobButtonPress _ -> 
          Wob.send left_pixmap#wob.w_top (WobIconifyRequest true)
      | _ -> ());
  left_pixmap#set_mask (ButtonPressMask:: left_pixmap#mask);
  left_pixmap#set_background !!twm_TitleBackground;
  left_pixmap#set_cursor !twm_ButtonCursor;
  left_pixmap#set_foreground !!twm_TitleForeground;  
  left_pixmap#set_borderpixel "white";
  left_pixmap#set_borderwidth 1;
  
  let right_pixmap = Pixmap.make sw resize_pixmap in
  right_pixmap#add_hook (fun e ->
      match e with
        WobButtonPress _ -> 
          User.twm_resize left_pixmap#wob.w_top true
      | _ -> ());
  right_pixmap#set_mask (ButtonPressMask:: right_pixmap#mask);
  right_pixmap#set_cursor !twm_ButtonCursor;
  right_pixmap#set_background !!twm_TitleBackground;  
  right_pixmap#set_foreground !!twm_TitleForeground;  
  right_pixmap#set_borderwidth 1;
  right_pixmap#set_borderpixel "white";
  
  let middle = Null.make () in
  middle#set_background !!twm_TitleBackground;
  middle#set_mask (ButtonPressMask :: middle#mask);
  middle#add_hook (fun e ->
      let w = middle#wob in
      match e with
        WobButtonPress e when Wob.click w.w_screen = Double ->
          let tw = w.w_top in
          Wob.send tw (WobMessage "minimize")
      | WobMessage "FocusIn" -> 
          middle#set_background !!twm_TitleForeground
      | WobMessage "FocusOut" -> 
          middle#set_background !!twm_TitleBackground
      | _ -> ());
  middle#set_actions (convert_bindings !!title_actions);
  label#set_mask (ButtonPressMask :: label#mask);
  label#set_actions (convert_bindings !!title_actions);
  let bar = Bar.make Horizontal [| Wob.desc left_pixmap;
      Wob.desc label;Wob.desc middle;  Wob.desc right_pixmap |]
  in
  bar#set_borderwidth !twm_titlebar_border;
  bar#set_background !!twm_TitleBackground;
  ([twm_hook;c_hook c; icon_manager_hook c], None, None, Some (bar :> wob_desc) , None)

let install_bindings () =
  Printf.printf "Twm.install_bindings: disabled ... ";
  print_newline ()
(*
  title_actions := !twm_TitleKeys;
  screen_actions := !twm_RootKeys;
  window_actions := !twm_WindowKeys;
  *)