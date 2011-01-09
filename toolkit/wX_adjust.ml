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
  
let v_total = 5000

type adjustement = {
    mutable v_pos : int;
    mutable v_inc : int;
    mutable v_page : int;
    mutable v_subjects : (unit -> unit) list;
  }

class t () =
  let adj  =
    {
      v_pos = 0;
      v_inc = 20;
      v_page = 1;
      v_subjects = [];
      }
  in
  object (self)

  val adj = adj
  val mutable validate = (fun _ -> true)
    
  method update_all =
    List.iter (fun f -> f ()) adj.v_subjects
    
  method update_x wg view =
    if view.x > wg.width - view.width then
      view.x <- max 0 (wg.width - view.width);
    adj.v_pos <- view.x * v_total / view.width;
    adj.v_page <- wg.width * v_total / view.width;
    self#update_all
  
  method update_y wg view =
    if view.y > wg.height - view.height then
      view.y <- max 0 (wg.height - view.height);
    adj.v_pos <- view.y * v_total / view.height;
    adj.v_page <- wg.height * v_total / view.height;        
    self#update_all
    
  method add_subject s = adj.v_subjects <- s :: adj.v_subjects
    
  method page_up = 
    let old_vpos = adj.v_pos in
    adj.v_pos <- max (adj.v_pos - adj.v_page) 0; 
    if old_vpos <> adj.v_pos && validate () then
      self#update_all
    else
      adj.v_pos <- old_vpos
      
  method page_down =
    let old_vpos = adj.v_pos in    
    adj.v_pos <- min (adj.v_pos + adj.v_page) (v_total - adj.v_page);
    if old_vpos <> adj.v_pos && validate () then
      self#update_all
    else
      adj.v_pos <- old_vpos
    
  method set_pos pos total =
    let total = max total 1 in
    let old_vpos = adj.v_pos in    
    adj.v_pos <- max 0 (min (v_total - adj.v_page) (pos * v_total / total));
    if old_vpos <> adj.v_pos && validate () then
      self#update_all
    else
      adj.v_pos <- old_vpos
    
  method down = 
    let old_vpos = adj.v_pos in    
    adj.v_pos <- min (adj.v_pos + adj.v_page / adj.v_inc)
    (v_total - adj.v_page);
    if  old_vpos <> adj.v_pos && validate () then
      self#update_all
    else
      adj.v_pos <- old_vpos
    
  method up = 
    let old_vpos = adj.v_pos in    
    adj.v_pos <- max (adj.v_pos - adj.v_page / adj.v_inc) 0;
    if  old_vpos <> adj.v_pos && validate () then
      self#update_all
    else
      adj.v_pos <- old_vpos

  method set_validity f = validate <- f
  method get_pos total = adj.v_pos * total / v_total
  method get_page total = adj.v_page * total / v_total
  method set_page page total = 
    let total = max total 1 in    
    let old_vpage = adj.v_page in
    adj.v_page <- page * v_total / total;
    if old_vpage <> adj.v_page then self#update_all
      
  method set_inc inc = 
    adj.v_inc <- inc;

  method set_params pos page total =
    let total = max total 1 in    
    let old_vpage = adj.v_page in
    let old_vpos = adj.v_pos in    
    adj.v_page <- page * v_total / total;
    adj.v_pos <- max 0 (min (v_total - adj.v_page) (pos * v_total / total));
    if old_vpos <> adj.v_pos && old_vpage <> adj.v_page then self#update_all
      
end
