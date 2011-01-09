(***********************************************************************)
(*                                                                     *)
(*                             WXlib                                   *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Xtypes
open WX_types

type desc = node list
and node = {
    mutable leaf_widget : contained;
    mutable leaf_subtree : contained option;
    mutable leaf_win : Xtypes.window;
    mutable leaf_closed : bool;
    mutable leaf_x : int;
    mutable leaf_y : int;
    mutable leaf_dx : int;
    mutable leaf_dy : int;
    mutable leaf_dy' : int;
    mutable leaf_dx' : int;
  }

let leaf offset contained = {
    leaf_widget = contained;
    leaf_subtree = None;
    leaf_win = noWindow;
    leaf_closed = true;
    leaf_x = offset;
    leaf_y = 0;
    leaf_dx = 0;
    leaf_dy = 0;
    leaf_dy' = 0;
    leaf_dx' = 0;
  }

let branch closed contained subtree = {
    leaf_widget = contained;
    leaf_subtree = Some subtree;
    leaf_win = noWindow;
    leaf_closed = closed;
    leaf_x = 0;
    leaf_y = 0;
    leaf_dx = 0;
    leaf_dy = 0;
    leaf_dy' = 0;
    leaf_dx' = 0;
  }

let rec iter f desc =
  match desc with
    [] -> ()
  | leaf :: tail ->
      f leaf.leaf_widget;
      match leaf.leaf_subtree with
        Some o -> f o; iter f tail
      | None -> iter f tail
          
let rec iter_visible f desc =
  List.iter (fun leaf ->
      f leaf.leaf_widget;
      if not leaf.leaf_closed then
        match leaf.leaf_subtree with
          Some o -> f o | None -> ()) desc

let rec iter_size desc dx dy =
  match desc with
    [] -> (dx,dy)
  | leaf :: tail ->
      let sz1 = leaf.leaf_widget#size_request in
      let borders = 2 * sz1.border_width in
      leaf.leaf_dx <- sz1.requested_width + leaf.leaf_x + borders;
      leaf.leaf_dy <- sz1.requested_height + borders;
      let _ =
        match leaf.leaf_subtree with
          Some o2 ->
            let sz2 = o2#size_request in
            let borders = 2 * sz2.border_width in
            leaf.leaf_dx' <- sz2.requested_width + borders;
            leaf.leaf_dy' <- sz2.requested_height + borders;
        | None -> ()
      in
      iter_size tail (max dx (max leaf.leaf_dx 
            (if leaf.leaf_closed then 0 else leaf.leaf_dx')))
      (dy + (max 10 leaf.leaf_dy) + 
          (if leaf.leaf_closed then 0 else leaf.leaf_dy'))
  
class t parent attributes =
  object (self)
  val mutable desc = []

  val xc_arrow = parent#cursor_make (FontCursor XC.xc_arrow) true

    inherit WX_object.t parent attributes as super
  
  method iter f = iter f desc
  method iter_visible f = iter_visible f desc
  
  method hide_desc = self#iter_visible (fun o -> o#hide)
  method destroy_desc = self#iter (fun o -> o#destroy)
  
  method set_desc d =
    List.iter (fun leaf -> 
        if not (leaf.leaf_win == noWindow) then
          begin
            X.destroyWindow s.s_display leaf.leaf_win;
            Eloop.remove_window s.s_eloop leaf.leaf_win
          end
    ) desc;
    desc <- d;    
    if w.w_shown then iter_visible (fun o -> o#show) d;
    w.w_clear <- true;
    self#wait_refresh true 0 0 0 0;
    self#wait_resize
  
  method size_request =     
    let sz = szhints in
    if not w.w_size_modified  || sz.comp_timestamp = s.s_timestamp then sz
    else let _ = () in
    sz.comp_timestamp <- s.s_timestamp;
    let (dx,dy) = iter_size desc 10  (2 * w.w_ipad_y) in
(*    List.iter (fun leaf ->
        let sz1 = leaf.leaf_widget#size_request in
        let borders = 2 * sz1.border_width in
        leaf.leaf_dx <- sz1.requested_width + leaf.leaf_x + borders;
        leaf.leaf_dy <- sz1.requested_height + borders;
        let _ =
          match leaf.leaf_subtree with
            Some o2 ->
              let sz2 = o2#size_request in
              let borders = 2 * sz2.border_width in
              leaf.leaf_dx' <- sz2.requested_width + borders;
              leaf.leaf_dy' <- sz2.requested_height + borders;
          | None -> ()
        in
        dx := max !dx (max leaf.leaf_dx 
            (if leaf.leaf_closed then 0 else leaf.leaf_dx'));
        dy := !dy + (max 10 leaf.leaf_dy) + 
          (if leaf.leaf_closed then 0 else leaf.leaf_dy');
    ) desc;
  *)
    sz.requested_width <- max (dx + (2 * w.w_ipad_x) + 10) sz.min_width;
    sz.requested_height <- max dy sz.min_height;
    sz
  
  method size_allocate x y dx dy =
    let g = w.w_geometry in
    let modified = w.w_size_modified || not (g.width = dx && g.height = dy) in
    w.w_size_modified <- false;
    super#size_allocate x y dx dy;
    if not modified then () else let _ = () in
    let x = w.w_ipad_x + 10 in
    let width = g.width - 2 * w.w_ipad_x in
    let height = g.height - 2 * w.w_ipad_y in
    let y = ref w.w_ipad_y in
    let rec iter desc =
      match desc with
        [] -> ()
      | leaf :: tail ->
          let sz1 = leaf.leaf_widget#size_request in
          let borders = 2 * sz1.border_width in
          let y' = !y + max 0 (10 - leaf.leaf_dy) in
          leaf.leaf_widget#size_allocate (x+leaf.leaf_x+sz1.border_width) (y'+sz1.border_width) (
            sz1.requested_width) (leaf.leaf_dy - 2 * borders);
          y := !y + max 10 leaf.leaf_dy;
          let _ =
            match leaf.leaf_subtree with
              Some o2 when  not leaf.leaf_closed ->
                o2#size_allocate x !y leaf.leaf_dx' leaf.leaf_dy';
                y := !y + leaf.leaf_dy'
            | _ -> ()
          in
          iter tail
    in
    iter desc
  
  method refresh =
    if not (s.s_timestamp > w.w_refresh_timestamp && not (w.w_window == noWindow))
    then () else let _ = () in
    super#refresh;
    let fg = w.w_foreground.c_pixel in
    let bg = w.w_background.c_pixel in
    let g = w.w_geometry in
    let gc = GCCache.get2 s.s_gcs fg bg in
    let draw = X.polySegment s.s_display w.w_window gc in
    let rec iter desc y dy =
      match desc with
        [] -> ()
      | leaf :: tail ->
          let h = max leaf.leaf_dy 10 in
          let mid = h/2 in
          match leaf.leaf_subtree with
            None ->
              let y' = y+dy+mid in
              draw [5, y, 5, y'; 5, y', (10+leaf.leaf_x), y'];
              iter tail y' (h - mid)
          | Some o2 ->
              let y' = y + dy + mid in
              let y1 = y' - 3 in
              let y2 = y' + 3 in
              draw (let list = [
                    5, y, 5, y1;
                    8, y', (10+leaf.leaf_x), y';
                    2, y1, 8, y1;
                    8, y1, 8, y2;
                    8, y2, 2, y2;
                    2, y2, 2, y1;
                    4, y', 6, y'] in
                if leaf.leaf_closed then 
                  (5, (y'-1), 5, (y'+1)):: list
                else list
              );
              if leaf.leaf_win == noWindow then
                begin
                  leaf.leaf_win <- X.createWindow s.s_display w.w_window
                    0 (y1-mid) 9 (y1+mid-1) copyDepthFromParent InputOnly 
                    copyVisualFromParent 0 [CWEventMask [ButtonPressMask];
                    CWCursor xc_arrow.curs_id
                    ];
                  X.mapWindow s.s_display leaf.leaf_win;
                  Eloop.add_window s.s_eloop leaf.leaf_win
                    (fun ev -> match ev.ev_event with
                        ButtonPressEvent _ ->
                          leaf.leaf_closed <- not leaf.leaf_closed;
                          if leaf.leaf_closed then
                            o2#hide else o2#show;
                          self#wait_resize;
                          self#wait_refresh true 0 0 0 0;
                      | _ -> ()
                  )
                end else
                Xlib.moveWindow s.s_display leaf.leaf_win 1 (y1-1);
              iter tail y2 (h - mid -3 + (
                  if leaf.leaf_closed then 0 else leaf.leaf_dy'))
    in
    iter desc w.w_ipad_y 0
 
end