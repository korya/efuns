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


(* Simple selection mecanism *)
  
open Xtypes
open Xlib

let selections = Hashtbl.create 11
  
let setSelection display window atom formatter time =
  Hashtbl.remove selections atom;
  Hashtbl.add selections atom formatter;
  X.setSelectionOwner display window atom time
  
let removeSelection display atom time =
  try 
    let _ = Hashtbl.find selections atom in
    Hashtbl.remove selections atom;
    X.setSelectionOwner display noWindow atom time
  with _ -> ()
  
module XSR = Xselectionrequest
module XS = Xselection
let handleSelectionRequest display sr =
  let prop = sr.XSR.property in
  let prop = if prop = noAtom then sr.XSR.target else prop in
  try
    let formatter = Hashtbl.find selections sr.XSR.selection in
    let format, value = formatter sr.XSR.target in
    X.changeProperty display sr.XSR.requestor PropModeReplace
      prop sr.XSR.target format value;
    X.sendEvent display sr.XSR.requestor false []
      (SelectionNotifyEvent {
        XS.time = sr.XSR.time;
        XS.requestor = sr.XSR.requestor;
        XS.selection = sr.XSR.selection;
        XS.target = sr.XSR.target;
        XS.property = sr.XSR.property;
      })
  with
    Not_found -> 
      X.sendEvent display sr.XSR.requestor false []
        (SelectionNotifyEvent {
          XS.time = sr.XSR.time;
          XS.requestor = sr.XSR.requestor;
          XS.selection = sr.XSR.selection;
          XS.target = sr.XSR.target;
          XS.property = noAtom;
        })

module XSC = Xselectionclear
let handleSelectionClear display sc =
  Hashtbl.remove selections sc.XSC.selection


let getSelection display window selection target time =
  try 
    let formatter = Hashtbl.find selections selection in
    let _, value = formatter target in
    value
  with _ ->
      let win = X.getSelectionOwner display selection in
      if win = noWindow then raise Not_found;
      let property = X.internAtom display "SELECTION_TRANSFERT" false in
      X.deleteProperty display window property;
      X.convertSelection display window selection target property time;
      let timer = ref false in
      Concur.Thread.add_timer 0.5 (fun () -> timer := true);
      let ev = waitPredEvent display (fun ev -> 
            if !timer then (* don't wait too much ! *)
              raise Not_found;
            match ev.ev_event with
              SelectionNotifyEvent s when s.XS.selection = selection -> true
            | _ -> false
        ) true in
      match ev.ev_event with
      | SelectionNotifyEvent s -> 
          if s.XS.property = noAtom then raise Not_found;
          let s = getWholeProperty display window property in
          s.gp_value
      | _ -> raise Not_found
