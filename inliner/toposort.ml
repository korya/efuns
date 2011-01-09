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

(*
Sort the functions according to the dependencies on other functions:
- level 0: leaf, no call to defined functions
- level 1: calls only to level 0 functions
- ...

  We use very simple algorithms. Performance is not an issue for now.
*)
open Asm
  
let sort_node program sorted new_sorted node =
  let rec iter i =
    if i < Array.length node.instrs then
      let instr = node.instrs.(i) in          
      begin
        match instr with
          { opcode = Call _; args = [ConstantBase (Const_symbol sub)]; } ->
            begin
              try
                let _ = Hashtbl.find program.desc sub in
                if List.mem sub sorted || List.mem sub new_sorted then
                  raise Exit
              with
                Not_found -> ()
            end
        | _ -> ()
      end;
      iter (i+1)
    else
    match node.link with
      { opcode = Switch nodes } ->
        List.iter Iter.add_node nodes
    | { opcode = Setuptrap (node,next) } ->
        Iter.add_node node;
        Iter.add_node next;
    | { opcode = Jmp; args = [ConstantBase (Const_label node)] } ->
        Iter.add_node node
    | { opcode = Jcond (comp,next); 
        args = [ConstantBase (Const_label node)] } ->
        Iter.add_node node;
        Iter.add_node next
    | { opcode = Poptrap next } ->
        Iter.add_node next
    | _ -> ()
  in
  iter 0
  
let sort program =
  let rec iter_level level left sorted = 
    let rec iter left new_sorted unsorted =
      match left with
      | global :: tail ->
          let desc = Hashtbl.find program.desc global in
          Iter.reset ();
          Iter.add_node desc.code;
          begin
            try
              Iter.iter (sort_node program tail new_sorted);
              iter tail (global :: new_sorted) unsorted
            with
              Exit ->
                iter tail new_sorted (global :: unsorted)
          end
      | [] -> 
          match unsorted with
            [] -> (level, new_sorted) :: sorted
          | _ -> iter_level (level+1) unsorted ((level,new_sorted) :: sorted)
    in
    iter left [] []
  in
  let list = iter_level 0 program.globals [] in
  List.rev list
  