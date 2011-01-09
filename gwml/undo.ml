(***********************************************************************)
(*                                                                     *)
(*                             GwML                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

let undo_list = ref []
  
class undo_manager =
  (object
  method undo = 
    match !undo_list with
      [] -> ()
    | f :: tail ->
        (try f () with _ -> ()); 
        (* prevent modification of the list during undo ... *)
        undo_list := tail;

  method add_undo f =
    undo_list := f :: !undo_list
end : Stdconfig.undo_manager)

let init () = Stdconfig.undo_manager := new undo_manager