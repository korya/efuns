(***********************************************************************)
(*                                                                     *)
(*                            AsmOpt                                   *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

type comparison =
    Ceq
  | Cne
  | Clt
  | Cle
  | Cgt
  | Cge
  
type signed_comparison =
    Isigned of comparison
  | Iunsigned of comparison

type stype = Object | Function | Unknown

  
let inverse_comp comp = match comp with
    Ceq -> Cne
  | Cne -> Ceq
  | Clt -> Cge
  | Cle -> Cgt
  | Cgt -> Cle
  | Cge -> Clt
    
  
let inverse_cond cond =
  match cond with
    Isigned c -> Isigned (inverse_comp c)
  | Iunsigned c -> Iunsigned (inverse_comp c)


 