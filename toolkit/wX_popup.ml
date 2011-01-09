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

type menu = item array
and item = label * action
and label = string
and action = (*
  SubMenu of menu
| Function of  *) handler

type submenu = <
    configure: base_attributes list -> unit;
show : unit; hide : unit;
popup : int -> int -> unit
  >


class t root desc =
  object (self)
  
  val root = root
  val mutable desc = desc
  val mutable realization = None
  val mutable button = None
  val mutable once = false
    
  method change d =
    desc <- d;
    match realization with
      None -> ()
    | Some top ->
        top#destroy;
        realization <- None

  method popup_once x y but =
    once <- true;
    self#popup x y but
        
  method popup x y (but : int option) =
    button <- but;
    let top = 
      match realization with
        Some (top : WX_top.t) -> 
          top#configure [Position (x,y)];
          top
      | None -> 
          let top = new WX_top.t root (Some (x,y)) [] in
          realization <- Some top;
          let vbar = new WX_bar.v top#container [] in
          top#container_add vbar#contained;
          for i = 0 to Array.length desc -1  do
            let (str, (action : action)) = desc.(i) in
            let label = new WX_label.t vbar#container str 
                  [IpadX 4; IpadY 4; Relief (ReliefRaisedN 2); MinWidth 100;
                  ExpandX true] in
            if str <> "" then
              label#configure [Bindings [
                  EnterWindow, (fun _ -> label#inverse);
                  LeaveWindow, (fun _ -> label#normal);
                  Button (anyButton, 0), (fun _ -> 
                      top#hide;
                      label#normal;
                      match button, !button_event with
                        None, 1 -> action ()
                      | _ ->  ();
                  );
                  ButtonReleased, (fun _ ->
                      top#hide;
                      label#normal;
                      match button, !button_event with
                        Some 1,1 | Some 2,2 | Some 3,3 -> 
                          (try action (); with _ -> ())
                      | _ -> ();
                  )
                ]];
            vbar#container_add label#contained;
          done;
          top#configure [Bindings 
              [EnterWindow, (fun _ ->
                  top#show;
                  (* si un sous-menu est actif, on doit aussi l'afficher. *));
              ButtonReleased, (fun _ -> 
              (* si un sous-menu est actif, on doit aussi le supprimer. *)
                  if once then top#destroy else top#hide);
              LeaveWindow, (fun _ -> 
              (* si un sous-menu est actif, on doit aussi le supprimer. *)
                  if once then top#destroy else top#hide);              
            ] 
          ];
          top
    in
    top#show;
    top#grab_pointer;
    
end
  
  