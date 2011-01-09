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



let langs = ref [
    "fr", [
      "Move", "Déplacer";
      "Resize", "Dimensions";
      "Iconify", "Iconifier";
    ];
  ]

let strings = Hashtbl.create 31
let use_lang lang =
  try
    List.iter (fun (w1, w2) -> Hashtbl.add strings w1 w2)
    (List.assoc lang !langs)
  with Not_found -> ()
  
  
  
let lang = try Sys.getenv "LANG" with Not_found -> ""

let _ = use_lang lang
      
let string name =
  try Hashtbl.find strings name with Not_found -> name