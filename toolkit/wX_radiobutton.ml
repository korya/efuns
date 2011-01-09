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

class orig parent (adj : WX_adjust.t) number attributes =
  object (self)
  
  inherit WX_button.orig parent attributes as super
  val number = number
  val adj = adj
    
  initializer
    adj#add_subject self#update_me
    
  method action () =
    adj#set_pos number WX_adjust.v_total
    
  method update_me () =
    if adj#get_pos WX_adjust.v_total = number then
      self#activate else self#desactivate
  
  method select =
    adj#set_pos number WX_adjust.v_total
end

class t = orig