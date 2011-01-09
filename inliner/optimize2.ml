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
open Approx  

  (* 
  Optimizations:
  
  *)

let stat_dead_instrs = ref 0
let stat_spill_deleted = ref 0
let stat_alloc_removed = ref 0
  
let add_use instr_used instr =  
  if instr.dead then 
    begin
      if !debug_opt2 then begin
      Printf.printf "%s USED BY DEAD %s" (
        SimplePrint.string_of_instr instr_used) (
        SimplePrint.string_of_instr instr);
          print_newline ();
        end;
      instr_used.used_by <- Cfg.remove2 instr instr.used_by;
      if instr_used.used_by = [] && !debug_opt2 then
        (Printf.printf "%d becomes dead" instr_used.instr_num; print_newline ());
    end
  else
    instr.use <- instr_used :: instr.use
    
let optimize2_instr instr = 
  List.iter (add_use instr) instr.used_by;
  instr.dead <- (instr.used_by = [])
  
let allow_alloc_from_dead = ref false
let merge_allocs node =
(*  Printf.printf "Allocations"; print_newline (); *)
  (*** Try to merge allocs ***)
  let instrs = node.instrs in
  let len = Array.length instrs in
  let rec iter_instr i allocs =    
    if i < len then
      let instr = instrs.(i) in
      match instr with
        { opcode = Alloc (n_init, n, frame, fast); args = [Register dst];
          leave_instr = leave_state; enter_instr = enter_state;
          used_by = used_by }
        ->
        (* first of all, is this alloc useful ? *)
          if
            try
              let v_orig = leave_state.registers.(regindex dst) in
              List.iter (fun i ->
                  match i with
                    { opcode = Movl; args = [_; OffsetBase(_, r)] }
                      when r = dst &&
                    (* ensure that the value in r cannot come from 
                      somewhere else *)
                    v_orig = i.leave_instr.registers.(regindex r)
                    -> ()
                  | _ -> raise Exit
              ) used_by;
              true
            with Exit -> false then begin
              if !debug_opt2 then begin
                  Printf.printf "UNUSED ALLOC %d" 
                    (node.node_ident.label); print_newline ();
                end;
              incr stat_alloc_removed;
              restart_analysis := true;
              instr.dead <- true;
              incr stat_dead_instrs;
              List.iter (fun i -> 
                  if !debug_opt2 then begin
                      Printf.printf "MARK [%s] AS DEAD" (SimplePrint.string_of_instr i);
                      print_newline ();
                    end;
                  incr stat_dead_instrs;
                  i.dead <- true) used_by;
              
            end else
        (* merge this alloc with preceding allocs *)
          begin
            let rec iter_allocs list =
              match list with
                [] -> 
                  iter_instr (i+1) [leave_state.registers.(regindex dst), 0, instr]
              | (v, offset, instr_alloc) :: tail ->
              (*** try to find a location for the address ***)
                  let rec iter_reg i =
                    if i < nregs then
                      if enter_state.registers.(i) = v 
                          && (enter_state.liveness.(i) <> [] ||
                          !allow_alloc_from_dead ||
                          (restart_analysis := true; 
                            true))
                      then
                        match instr_alloc with
                          { opcode = Alloc(n_orig, n_current, frame, fast) } ->
                            instr_alloc.opcode <- 
                            Alloc(n_orig, n_current+n, frame, fast);
                            instr.opcode <- Leal;
                            instr.args <- [ 
                              OffsetBase(Const_int (n_current - offset), regs.(i)); Register dst];
                            instr.directives <- [Dir_noalloc n_init];
                            incr stat_alloc_removed;
                            iter_instr (i+1) 
                            ((leave_state.registers.(regindex dst), n_current,
                                instr_alloc) :: allocs)
                        | _ -> assert false
                      else
                        iter_reg (i+1)
                    else
                      iter_allocs tail
                  in
                  iter_reg 0                    
            in
            iter_allocs allocs
          end
      | { opcode = Call (Some _) }
      | { opcode = Alloc _ } -> iter_instr (i+1) []
      | _ -> iter_instr (i+1) allocs
  in
  iter_instr 0 []

let optimize2_node node = 
  if node.link.used_by = [] then
    (Printf.printf "LINK DEAD [%d]%s" node.link.instr_num (SimplePrint.string_of_instr node.link); 
      print_newline ());
  Array.iter optimize2_instr node.instrs;
  if node.link.used_by = [] then
    (Printf.printf "LINK DEAD !"; print_newline ());
  optimize2_instr node.link;
  merge_allocs node  
  
let remove_dead node =
  let instrs = ref [] in
  let len = Array.length node.instrs in
  for i = 0 to len - 1 do
    let instr = node.instrs.(len-1-i) in
    if (not instr.dead) || !dont_remove_dead then
      instrs := instr :: !instrs
    else
      ( incr stat_dead_instrs;
        restart_analysis := true;
        if !debug_opt2 then
          (Printf.printf "Remove DEAD %s" 
            (SimplePrint.string_of_instr instr); 
            print_newline ()));
  done;
  node.instrs <- Array.of_list !instrs
  
let func func = 
  if !debug_opt2 then begin
      Printf.printf "****************************** Optimizing(2) %s" func.fun_name;
      print_newline ();
    end;
  (* Mark dead instructions, and compute "use" fields *)
  Iter.reset ();
  Iter.add_node func.code;
  Iter.iter_nodes optimize2_node;

  if !Optimize3.try_speculative_reloads then
    Optimize3.func func
  else
    begin      
      
       (* Remove dead instructions *)
      Iter.reset ();
      Iter.add_node func.code;
      Iter.iter_nodes remove_dead;
          
  (* some peephole optimization *)
      if not !no_peephole then
        begin
          Iter.reset ();
          Iter.add_node func.code;
          Iter.iter_nodes Peephole.iter_node
        end;
    end
    