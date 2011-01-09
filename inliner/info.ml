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

open Args
open Asm

let side_effect = ref false
let side_effects = ref []

let no_se_prims = Hashtbl.create 127

  (* 
  fun_side_effect: the function can produce a side effect. As a consequence,
  it should not be removed from a computation.
  fun_functional: indicate if the function may modify some of the blocks in
  the heap. Even if a function is functional, it might still modify some
  c values, or have side effects... but not break any optimizations.
*)
  
let _ =
(* no side effect *)
  List.iter (fun prim -> Hashtbl.add no_se_prims prim false)
  ["make_vect";
    "caml_open_descriptor";
    "sys_open";
    "format_float";
    "format_int";
    "string_equal";
    "create_string";
    "greaterequal";
    "lessequal";
    "equal";
    "is_printable";
    "sys_get_argv";
    "hash_univ_param";
    "marshal_data_size";
    "output_value_to_string";
    "obj_is_block";
    "compare";
    "md5_string";
    "obj_block";
    "lessthan";
    "weak_create";
    "weak_get";
    "caml_alloc";
    "sys_get_config";
    "obj_tag";
    "sys_date";
    "weak_check";
    "alloc_dummy";
  ];
  (* side effect *)
  List.iter (fun prim -> Hashtbl.add no_se_prims prim true)
  [
    "caml_flush";
    "caml_output_char";
    "caml_input";
    "output_value";
    "caml_seek_out";
    "caml_pos_out";
    "caml_channel_size";
    "caml_set_binary_mode";
    "sys_exit";
    "caml_output";
    "caml_output_int";
    "caml_input_char";
    "caml_input_int";
    "caml_seek_in";
    "caml_pos_in";
    "caml_close_channel";
    "caml_input_scan_line";
    "input_value";
    "install_signal_handler";
    "output_value_to_buffer";
    "register_named_value";
    "weak_set";
  ]

let is_se s =
  try
    let func = try Hashtbl.find program.desc s with _ ->
          Hashtbl.find program.env s in
    side_effect := !side_effect || func.fun_side_effect;
    match func.fun_functional with
      None -> ()
    | Some [] -> raise Exit
    | Some list -> side_effects := list @ !side_effects
  with Not_found -> try
        side_effect := !side_effect || Hashtbl.find no_se_prims s
      with Not_found ->
          Printf.printf "Unknown %s" s;
          print_newline (); raise Not_found
          
let se_node node =
  let instrs = Array.to_list node.instrs in
  let rec iter list =
    match list with
      [] -> ()
    | { opcode = Movl; args = [Const(Const_symbol s); Register Eax] } ::
      { opcode = Call _; args = [ConstantBase(Const_symbol "caml_c_call")] }
        :: tail -> 
        is_se s;
        iter tail
    | { opcode = Addl; args = [Const(Const_int n); Register r1] } ::
      { opcode = Pushl; args = [Register r2] } ::
      { opcode = Call _; args = [ConstantBase(Const_symbol "modify")] } ::
      tail when r1 = r2 ->
        side_effects := (n/4) :: !side_effects;
        iter tail
    | { opcode = Lea; args = [_; Register r1] } ::
      { opcode = Pushl; args = [Register r2] } ::
      { opcode = Call _; args = [ConstantBase(Const_symbol "modify")] } ::
      tail when r1 = r2 ->
        raise Exit
    | { opcode = Call _; args = [ConstantBase(Const_symbol "modify")] } ::
      tail ->
        side_effects := 0 :: !side_effects;
        iter tail
    | { opcode = Call _; args = [ConstantBase(Const_symbol s)] } :: tail ->
        is_se s;
        iter tail
    | { opcode = Call _ } :: _ -> raise Exit
    | _ :: tail -> 
        iter tail
  in
  iter instrs;
  match node.link with
    { opcode = Jmp;
      args = [ConstantBase(Const_symbol s)] } -> is_se s
  | { opcode = Jcond _;
      args = [ConstantBase(Const_symbol s)] } -> is_se s
  | { opcode = Raise } ->
      side_effect := true
  | { opcode = Jmp; args = [ConstantBase(Const_label _ )] } -> ()
  | { opcode = Jmp } -> raise Exit
  | _ -> ()
  
let se_func func =
  side_effect := false;
  side_effects := [];
  func.fun_side_effect <- false; (* in case of self iter *)
  func.fun_functional <- None;
  Iter.reset ();
  Iter.add_node func.code;
  (try Iter.iter_nodes se_node;
      func.fun_side_effect <- !side_effect;
      match !side_effects with
        [] -> func.fun_functional <- None
      | list -> func.fun_functional <- Some list
    with _ -> 
        func.fun_functional <- Some [];
        func.fun_side_effect <- true);
  Printf.printf "%s: " func.fun_name;
  (match func.fun_functional with
      None -> Printf.printf "%s is functional" func.fun_name
    | Some [] -> Printf.printf "%s IS NOT functional" func.fun_name
    | Some list -> 
        Printf.printf "%s is not functional on fields" func.fun_name;
        List.iter (fun i -> Printf.printf " %d" i) list);
  if func.fun_side_effect then Printf.printf " [Side effect]";
  print_newline ()

let rec compute_se sorted = ()
  (* This computation is false. We could force the compiler to give some 
  information. In fact, it is false because we don't take movls into
  account, only "modify", but the latter ones are not used for integers ... 
  
  match sorted with
    [] -> ()
  | (i, list) :: tail ->
      Printf.printf "Compute side effect at level %d" i;
      print_newline ();
      List.iter (fun s ->
          se_func (Hashtbl.find program.desc s)) list;
  compute_se tail
  *)