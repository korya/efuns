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
  
let broadcast wobs e =  
  for i = 0 to Array.length wobs - 1 do
    Wob.send wobs.(i) e
  done

  (* GetSize:
  x : AND between 1: extensible, 2: retractable
  y : AND between 1: extensible, 2: retractable
  width : optimal width
  height : optimal height
  *)

let set_shape w wobs =
  let module S = Shape in
  let module X = S.X in
  let tw = w in
  let init = ref S.Set in
  Array.iter (fun w ->
      let g = w.w_geometry in
      X.shapeCombine display tw.w_window S.Bounding 
        (g.x+g.border) (g.y+g.border) w.w_window
        S.Bounding !init; 
      init := S.Union
  ) wobs
  
class bar sens descrs = 
  object (self)
  inherit wob_base as super
  
  val mutable descrs = descrs
  val mutable wobs = [||]
  val mutable sizes = [||]
    
  method first_hook e = 
    let w = self#wob in
    match e with
    | WobInit ->
        wobs <- Array.map (fun desc ->
            Wob.make w desc
        ) descrs
    
    | WobGetSize ->
        broadcast wobs WobGetSize;
        sizes <- Array.map (fun w -> 
            let g = w.w_geometry in
            { x=g.x; y = g.y;
              width = g.width; height = g.height;
              border = g.border;
            }) wobs;
        let g = w.w_geometry in
        g.x <- extensible_width;
        g.y <- extensible_height;
        g.width <- self#min_width;
        g.height <- self#min_height;
        g.border <- self#borderwidth;
        Array.iter (fun r ->
            let width = r.width + 2 * r.border in
            let height = r.height + 2 * r.border in
            match sens with
              Vertical -> 
                if width > g.width then g.width <- width;
                g.y <- g.y lor r.y;
                g.height <- g.height + height
            | Horizontal ->
                if height > g.height then g.height <- height;
                g.x <- r.x lor g.x;
                g.width <- g.width + width
        ) sizes    
    
    | WobCreate ->
        self#create true;
        X.mapWindow display w.w_window;
        broadcast wobs WobCreate;
        is_shaped <- false;
        Array.iter (fun w -> is_shaped <- is_shaped || w.w_oo#is_shaped) wobs
    
    | WobUpdateShape ->
        broadcast wobs WobUpdateShape;
        if is_shaped then set_shape w wobs
          
    | WobResize _ ->
        let g = w.w_geometry in
        let b_width = g.width in
        let b_height = g.height in
        let holes = ref 0 in
        let required = ref 0 in
        let removable = ref 0 in
        let retractables = ref 0 in
        begin
          match sens with
            Vertical ->
              for i = 0 to Array.length wobs - 1 do
                let r = sizes.(i) in
                let height = r.height + 2 * r.border in
                if r.y land 1 = 1 then incr holes;
                if r.y land 2 = 2 then removable := !removable + height;
                required := !required + height;
              done;
              let left = b_height - !required in
              let y = ref 0 in
              for i = 0 to Array.length wobs - 1 do
                let r = sizes.(i) in
                let height = r.height + 2 * r.border in
                let w = wobs.(i) in
                let g = w.w_geometry in
                g.x <- 0;
                g.width <- b_width - 2 * r.border;
                let dy = height + (
                    if left>0 && r.y land 1 = 1 then left / !holes else
                    if left<0 && r.y land 2 = 2 then
                      - ((min (-left) !removable) * height) / !removable
                    else 0) in
                g.height <- dy - 2 * r.border;
                g.y <- !y;
                y := !y + dy;
              done;
          |  Horizontal ->
              for i = 0 to Array.length wobs - 1 do
                let r = sizes.(i) in
                let width = r.width + 2 * r.border in                
                if r.x land 1 = 1 then incr holes;
                if r.x land 2 = 2 then removable := !removable + width;
                required := !required + width;
              done;
              let left = b_width - !required in
              let x = ref 0 in
              for i = 0 to Array.length wobs - 1 do
                let r = sizes.(i) in
                let width = r.width + 2 * r.border in
                let w = wobs.(i) in
                let g = w.w_geometry in
                g.y <- 0;
                g.height <- b_height - 2 * r.border;
                let dx = width + (
                    if left>0 && r.x land 1 = 1 then left / !holes else
                    if left<0 && r.x land 2 = 2 then
                      - ((min (-left) !removable) * width) / !removable
                    else 0) in
                g.width <- dx - 2 * r.border;
                g.x <- !x;
                x := !x + dx;
              done;
        end;
        
        (if w.w_window <> noWindow then
            let g = w.w_geometry in
            Xlib.moveResizeWindow display w.w_window g.x g.y 
            (max 1 g.width) (max 1 g.height));
        broadcast wobs (WobResize false);
    
    | WobDestroy ->
        broadcast wobs WobDestroy;
        if w.w_window <> noWindow then
          let s = w.w_screen in
          Eloop.remove_window s.s_scheduler w.w_window;
          X.destroyWindow display w.w_window;
          w.w_window <- noWindow
    
    | WobEnter 
    | WobLeave _
    | WobKeyPress _
    | WobButtonRelease _ 
    | WobButtonPress _ -> () (* Should be discarded immediatly *)
    | _ -> broadcast wobs e
  
  method insert_item i desc =
    let size = Array.length descrs in
    if i > size then failwith "Bar: insert_item after end of bar";
    descrs <- Array.init (size+1) (fun j ->
        if i=j then desc else
        if j < i then descrs.(j) else descrs.(j-1));
    match wob with
      None -> ()
    | Some bw ->
        wobs <- Array.init (size+1) (fun j ->
            if i=j then
              (let w = Wob.make bw desc in
                if bw.w_window <> noWindow then
                  (Wob.send w WobGetSize;
                    Wob.send w WobCreate);
                w)
            else
            if j < i then wobs.(j) else wobs.(j-1));
        Wob.send_one bw.w_top WobGetSize
  
  method insert_items i descs =
    let size = Array.length descrs in
    let adds = Array.length descs in
    if adds>0 then begin
        if i > size then failwith "Bar: insert_item after end of bar";
        descrs <- Array.init (size+adds) (fun j ->
            if i<=j && j<i+adds then descs.(j-i) else
            if j < i then descrs.(j) else descrs.(j-adds));
        match wob with
          None -> ()
        | Some bw ->
            wobs <- Array.init (size+adds) (fun j ->
                if i<=j && j<i+adds then
                  (let w = Wob.make bw descs.(j-i) in
                    if bw.w_window <> noWindow then
                      (Wob.send w WobGetSize;
                        Wob.send w WobCreate);
                    w)
                else
                if j < i then wobs.(j) else wobs.(j-adds));
            Wob.send_one bw.w_top WobGetSize
      end
      
  method remove_item i =
    let size = Array.length descrs in
    if i >= size then failwith "Bar: remove_item after end of bar";
    descrs <- Array.init (size-1) (fun j ->
        if j >= i then descrs.(j+1) else descrs.(j));
    match wob with
      None -> ()
    | Some w ->
        wobs <- Array.init (size-1) (fun j ->
            if i = j then Wob.send wobs.(i) WobDestroy;
            if j >= i then wobs.(j+1) else wobs.(j));
        Wob.send_one w.w_top WobGetSize
        
  method add_item desc = self#insert_item (self#nitems) desc
  method nitems = Array.length descrs
  method items = descrs    
    
  method reverse =
    let hilitep = super#reverse in
    Array.iter (fun desc -> desc#set_hilite hilitep) descrs;
    hilitep
    
  method iter f =
    f (self :> wob_desc);
    Array.iter (fun desc -> desc#iter f) descrs
    
end

let make sens descrs = new bar sens descrs
  
  