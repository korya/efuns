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
  
type top_internals = {
    mutable center : wob option ;
    mutable left : wob option;
    mutable right : wob option;
    mutable title : wob option;
    mutable bottom : wob option;
    borders : geometry;
  }

let client_mask = [
    StructureNotifyMask;
    PropertyChangeMask;
    VisibilityChangeMask; 
    EnterWindowMask; 
    LeaveWindowMask; 
    ColormapChangeMask; 
    FocusChangeMask]

let top_mask =
  [StructureNotifyMask;PropertyChangeMask;
    VisibilityChangeMask ;EnterWindowMask ; 
    LeaveWindowMask; SubstructureRedirectMask;
    SubstructureNotifyMask; ButtonPressMask; KeyPressMask;
    ColormapChangeMask ; FocusChangeMask]
  
let notify_mode mode =
  match mode with
  | NotifyNormal -> "NotifyNormal"
  | NotifyGrab -> "NotifyGrab"
  | NotifyUngrab -> "NotifyUngrab"
  | NotifyWhileGrabbed -> "NotifyWhileGrabbed"
    
let notify_detail detail =
  match detail with
  | NotifyAncestor -> "NotifyAncestor"
  | NotifyVirtual -> "NotifyVirtual"
  | NotifyInferior -> "NotifyInferior"
  | NotifyNonlinear -> "NotifyNonlinear"
  | NotifyNonlinearVirtual -> "NotifyNonlinearVirtual"
  | NotifyPointer -> "NotifyPointer"
  | NotifyPointerRoot -> "NotifyPointerRoot"
  | NotifyDetailNone -> "NotifyDetailNone"

let ssend opt e =
  match opt with
    None -> ()
  | Some w -> Wob.send w e

let broadcast d e =
  ssend d.center e;
  ssend d.left e;
  ssend d.right e;
  ssend d.title e;
  ssend d.bottom e

open Options
let no_borders_with_shape = define_option ["no_borders_with_shape"] ""
    bool_option true
  
let set_shape w deco =
  let module S = Shape in
  let module X = S.X in
  let tw = w in
  let init = S.Set in
  let init = let g = w.w_geometry in
    if g.border > 0 && not !!no_borders_with_shape then begin
        X.shapeRectangles display w.w_window S.Bounding 0 0 [
          - g.border, - g.border, 
          g.width + 2 * g.border, g.height + 2 * g.border
          ] init UnSorted;
        X.shapeRectangles display w.w_window S.Bounding 0 0 
          [
          0,0, g.width, g.height
          ] S.Substract UnSorted; 
        S.Union
      end else init      
  in
  let init = match deco.center with None -> init | Some w -> 
        let g = w.w_geometry in
(*
        X.shapeRectangles display w.w_window S.Bounding 0 0
          [-g.border, -g.border, g.width+2*g.border+1, g.height+2*g.border+1
        ] S.Union UnSorted; 
        *)
        X.shapeCombine display tw.w_window S.Bounding 
          (g.x+g.border) (g.y+g.border)
        w.w_window S.Bounding init; 
        S.Union in
  let init = match deco.left with None -> init | Some w -> 
        let g = w.w_geometry in
        X.shapeCombine display tw.w_window S.Bounding 
          (g.x+g.border) (g.y+g.border)
          w.w_window S.Bounding init; S.Union in
  let init = match deco.right with None -> init | Some w -> 
        let g = w.w_geometry in
        X.shapeCombine display tw.w_window S.Bounding
          (g.x+g.border) (g.y+g.border)
          w.w_window S.Bounding init; S.Union in
  let init = match deco.title with None -> init | Some w -> 
        let g = w.w_geometry in
        X.shapeCombine display tw.w_window S.Bounding
          (g.x+g.border) (g.y+g.border)
          w.w_window S.Bounding init; S.Union in
  let init = match deco.bottom with None -> init | Some w -> 
        let g = w.w_geometry in
        X.shapeCombine display tw.w_window S.Bounding
          (g.x+g.border) (g.y+g.border)
        w.w_window  S.Bounding init; S.Union in
  ()

let size name ow =
  match ow with
    None -> ()
  | Some w ->
      let g = w.w_geometry in
      Log.printf "Inside: %s\n" name;
      Log.printf "x= %d\n" g.x;
      Log.printf "y= %d\n" g.y;
      Log.printf "dx= %d\n" g.width;
      Log.printf "dy= %d\n" g.height;
      Log.printf "border= %d\n" g.border


let top_list = ref []

class top deco name =
  object (self)
    inherit wob_base as super
    
    val mutable client = (None: client_desc option)
    val deco = deco
    val over = (name = "")
    val name = name
    
    method deco = deco
    method set_client c = client <- Some c
    method client = match client with None -> raise Not_found | Some c -> c
    
    method is_shaped = is_shaped
    
    method iter f =
      f (self :> wob_desc);
      let d = deco in
      List.iter (fun d ->
          match d with None -> () | Some w ->
              w.w_oo#iter f) [d.center; d.left; d.right; d.title; d.bottom]    
    
    method mask =
      if over then super#mask else client_mask @ super#mask
    
    method  first_hook e = 
      let w = self#wob in
      match e with
        WobGetSize -> 
          broadcast deco WobGetSize;
          let width,height = match  deco.center with
              None -> assert false
            | Some w -> 
                let g = w.w_geometry in
                g.width + 2 * g.border,
                g.height + 2 * g.border in
          deco.borders.x <- (match deco.left with None->0
            | Some w-> let g = w.w_geometry in g.width + 2 * g.border);
          deco.borders.y <- (match deco.title with None->0
            | Some w-> let g = w.w_geometry in g.height+2*g.border);
          deco.borders.width <- (match deco.right with None->0
            | Some w-> let g = w.w_geometry in g.width + 2 * g.border);
          deco.borders.height <- (match deco.bottom with None->0
            | Some w-> let g = w.w_geometry in g.height+2*g.border);
          w.w_geometry.width <- width + deco.borders.x + deco.borders.width;
          w.w_geometry.height <- height + deco.borders.y + deco.borders.height;
          Wob.send w (WobResize false);
      
      | WobResize force ->
          let borders = deco.borders in
          let tg = w.w_geometry in
          let ldx = borders.x in
          let rdx = borders.width in
          let tdy = borders.y in
          let bdy = borders.height in
          let dx = tg.width - ldx - rdx in
          let dy = tg.height - tdy - bdy in
          let cw = match deco.center with None -> assert false | Some w->w in
          let cg = cw.w_geometry in
          
          cg.x <- ldx;
          cg.y <- tdy;
          cg.width <- dx - 2 * cg.border;
          cg.height <- dy - 2 * cg.border;        
          begin
            match deco.left with
              None -> ()
            | Some w -> let g = w.w_geometry in
                g.x <- 0;
                g.y <- tdy;
                g.width <- ldx - 2 * g.border;
                g.height <- dy - 2 * g.border;
          end;
          begin
            match deco.right with
              None -> ()
            | Some w -> let g = w.w_geometry in
                g.x <- dx + ldx;
                g.y <- tdy;
                g.width <- rdx - 2 * g.border;
                g.height <- dy - 2 * g.border;        
          end;
          begin
            match deco.title with
              None -> ()
            | Some w -> let g = w.w_geometry in
                g.x <- 0;
                g.y <- 0;
                g.width <- tg.width - 2 * g.border;
                g.height <- tdy - 2 * g.border;        
          end;
          begin
            match deco.bottom with
              None -> ()
            | Some w -> let g = w.w_geometry in
                g.x <- 0;
                g.y <- tdy + dy;
                g.width <- tg.width - 2 * g.border;
                g.height <- bdy - 2 * g.border;        
          end;
          (*
          Log.printf "RESIZE: %s\n" "";
          Log.printf "Width %d\n" tg.width;
          Log.printf "Height %d\n" tg.height;
          Log.printf "Border %d\n" tg.border;
          size "Center" deco.center;
          size "Left" deco.left;
          size "Right" deco.right;
          size "Top" deco.title;
          size "Bottom" deco.bottom;
          *)
          if w.w_window <> noWindow then begin
              if over then begin
                  Xlib.moveResizeWindow display w.w_window 
                    tg.x tg.y tg.width tg.height 
                end
              else begin
                  Xlib.resizeWindow display w.w_window tg.width tg.height;
                  let s = w.w_screen in
                  let (c,cw) = Wintbl.find s.s_clients w.w_window in
                  let cg = cw.w_geometry in
                  let ctg = cw.w_top.w_geometry in
                  cg.width <- tg.width;
                  cg.height <- tg.height;
                  Wob.send cw.w_top WobGetSize
                end
            end;
          broadcast deco (WobResize force);
          if is_shaped then Wob.send_one w WobUpdateShape
      
      | WobUpdateShape ->
          broadcast deco WobUpdateShape;
          if is_shaped then set_shape w deco
            
      | WobCreate -> 
          let tw = w in
          let g = tw.w_geometry in
          Wob.setenv w is_mapped false;
          self#create over;
          if not over then
            begin
              Icccm.setWM_NAME display w.w_window name;
              Icccm.setWM_CLASS display w.w_window ["gwml_"^name;"GwML"];
            end;
          let shaped wo = match wo with
              None -> false | Some w -> w.w_oo#is_shaped in
          broadcast deco WobCreate;
          is_shaped <- (shaped deco.title) || (shaped deco.bottom) ||
          (shaped deco.center) || (shaped deco.left) || (shaped deco.right);
          if is_shaped then Wob.send_one w WobUpdateShape;

      | WobMap  -> 
          let g = w.w_geometry in
          if over then begin
              Xlib.moveWindow display w.w_window g.x g.y;
            end;
          X.mapWindow display w.w_window; 
          Wob.setenv w is_mapped true;
          broadcast deco WobMap;
          (* Wob.send w WobRaiseWindow *)
        
      | WobUnmap iconify ->      
          X.unmapWindow display w.w_window;
          Wob.setenv w is_mapped false;
          broadcast deco (WobUnmap iconify)
      
      | WobDestroy ->
          let s = w.w_screen in
          broadcast deco WobDestroy;
          if w.w_window <> noWindow then
            begin
              Eloop.remove_window s.s_scheduler w.w_window;
              X.destroyWindow display w.w_window;
              w.w_window <- noWindow
            end
      | WobMove ->
          let g = w.w_geometry in
          broadcast deco e;
          Xlib.moveWindow display w.w_window g.x g.y
      | WobKeyPress (e,s,k) -> self#handle_key (e,s,k)
      | WobButtonPress e ->  self#handle_button e
      | WobButtonRelease _ 
      | WobEnter
      | WobLeave _ -> () (* discard these local events *)
      | WobRaiseWindow -> () (* Xlib.raiseWindow display w.w_window *)
      | WobLowerWindow -> Xlib.lowerWindow display w.w_window
      | _ -> broadcast deco e
end

let last_hook w e = ()
      
let make sw name hooks center left right top bottom =
  let sw = sw.w_top.w_parent in
  let deco = {
      center = None; 
      left = None;
      right = None;
      title = None;
      bottom = None;
      borders = { x=0; y=0; width=0; height=0; border=0; };          
    } 
  in
  let g = { x=0; y=0; width=0; height=0; border=0; } in
  let desc = new top deco name  in
  let rec w = {
      w_window = noWindow;
      w_parent = sw;
      w_top = w;
      w_screen = sw.w_screen;
      w_geometry = g;
      w_env = Wobenv.empty ();
      w_oo = (desc :> Gwml.wob_desc);
      w_queue = [];
    } 
  in
  desc#set_wob w;
  let g = w.w_geometry in
  List.iter (fun hook -> desc#add_hook (hook desc)) hooks;
  send w WobInit;
  let some opt = match opt with None->None | 
      Some wob -> Some(Wob.make w wob) in
  deco.center <- Some (Wob.make w center);
  deco.left <- some left;
  deco.right <- some right;
  deco.title <- some top;
  deco.bottom <- some bottom;
  let s = w.w_screen in
  exec_hooks w s.s_top_opening_hooks;
  Wob.send w WobGetSize;
  Wob.send w WobCreate;
  desc
    
(* Resize client wob *)  
let resize w dx dy = 
  let g = w.w_geometry in
  g.width <- dx;
  g.height <- dy;
  Wob.send_one w.w_top WobGetSize

(* Resize top window *)  
let resize_top w dx dy =
  w.w_geometry.width <- dx;
  w.w_geometry.height <- dy;
  Wob.send_one w (WobResize false)
  

type window_desc = 
  (top -> Gwml.wob_event -> unit) list * 
  Gwml.wob_desc option * 
  Gwml.wob_desc option *
  Gwml.wob_desc option * 
  Gwml.wob_desc option
