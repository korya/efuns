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
Ce que l'on veut absolument supprimer:
- 
try
 ...; raise Exit
with
Exit -> (1) ...

doit devenir:

try
...; Jump (1)
with
Exit -> (1) ...

- Les cascades de matchs (generee's par les cas communs) doivent etre
mis en un seul match.
- Les fonctions inlinees doivent etre simplifiees.  

************

LES MOVES INUTILES:

(I1)  movl %r1, %r2
...
(I2)  op %r2, ...

when
use_by(I1, %r2) = U &&
for_all(I in U, value(I, %r1) = value(I, %r2))
*)

open Args
open Asm
open Approx
  
(* The analysis: *)

  
let clean_node node =
  node.enter_node <- noapprox;
  node.leave_node <- noapprox;
  node.back_edges <- [];
  
  Array.iter (fun instr ->
      instr.enter_instr <- noapprox;
      instr.leave_instr <- noapprox;
      instr.used_by <- [];
      instr.use <- [];
      instr.dead <- false;
  ) node.instrs;
  let instr = node.link in
  instr.enter_instr <- noapprox;
  instr.leave_instr <- noapprox;
  instr.used_by <- [];
  instr.use <- [];
  instr.dead <- false

  
let initialize func = 
  func.fun_size <- 0;
  func.ret_nodes <- [];
  
(* Set all node.enter_node to noapprox *)
  Iter.reset ();
  Iter.add_node func.code;
  Iter.iter_nodes clean_node;
  
  (* Ensure that the back-edge information is reliable *)
  Iter.reset ();
  Iter.add_node func.code;
  (* Ensure there is at least one in-edge *)
  Cfg.add_edge program.start_node func.code;
  Iter.iter_nodes (fun node ->
      let succ = node_succ node in
      List.iter (Cfg.add_edge node) succ;
      let len = Array.length node.instrs in
      func.fun_size <- func.fun_size + len + 1;
      match node.link with
      | { opcode = Jmp; args = [ ConstantBase (Const_symbol _)] } 
      | { opcode = Jmp; args = [ Indirect (Register _)] } 
      | { opcode = (Ret | Raise) } ->
          func.ret_nodes <- node :: func.ret_nodes;
      | _ -> ()
  );
  (* fill a table with all nodes leading to an end. *)
  let table = Hashtbl.create 31 in
  Hashtbl.add table program.start_node.node_ident.label ();
  let rec iter node =
    try
      Hashtbl.find table node.node_ident.label
    with Not_found ->
        Hashtbl.add table node.node_ident.label ();
        List.iter iter node.back_edges
  in
  List.iter iter func.ret_nodes;
  Iter.reset ();
  Iter.add_node func.code;
  Iter.iter_nodes (fun node ->
      try
        Hashtbl.find table node.node_ident.label
      with
        Not_found ->
          func.ret_nodes <- node :: func.ret_nodes;
          iter node
  );
        
  if func.ret_nodes = [] then
      assert (func.code.back_edges <> [])
      (* These back-edges seem good to start with since we know what is
    live through them. *)
  
let approximation func = Approx.compute func
  
let liveness func = Liveness.compute func
let optimization_2 func = ()
  
let output = ref stdout
  
let rec analysis func =
  if func.fun_debug then
    Print_cfg.print_func !output func;
  restart_analysis := false;
  if !debug then
    (Printf.printf "Analysis %s" func.fun_name; 
      print_newline ());
  initialize func;
  approximation func;
  Optimize1.func func;
  liveness func;
  Optimize2.func func;
  if func.fun_debug &&
    !one_debug && !debug_print_code then begin
      Printf.printf "Code after pass:";
      print_newline ();
      print_func func;
    end;
  if !restart_analysis then analysis func
    
let one_analysis func =
  if func.fun_debug then
    Print_cfg.print_func !output func;
  restart_analysis := false;
  if !debug then
    (Printf.printf "Analysis %s" func.fun_name; 
      print_newline ());
  initialize func;
  approximation func;
  liveness func
    
let analysis func =
  
  if func.fun_debug then
    output := Print_cfg.create_file func;
  
  analysis func;
  (** At this point, no dead instruction is left *)
  
  Optimize4.func func;
  (** Some edges may not be OK ... *)
  
  if !restart_analysis then one_analysis func;
    
  (* unroll simple loops *)
  Iter.reset ();
  Iter.add_node func.code;
  Iter.iter_before_nodes Optimize4.unroll_node;
  
  if !restart_analysis then begin      
      if !one_debug && !debug_print_code
      then begin
          Printf.printf "Code before pass:";
          print_newline ();
          print_func func;
        end;
      analysis func;
    end;
  
  Optimize2.allow_alloc_from_dead := true;
  (try
      Iter.reset ();
      Iter.add_node func.code;
      Iter.iter_nodes Optimize2.merge_allocs;
    with e ->
        Printf.printf "Exception %s" (Printexc.to_string e);
        print_newline ();
  );
  Optimize2.allow_alloc_from_dead := false;
  if !restart_analysis then analysis func;
  
  if !one_debug && func.fun_debug then begin
      Printf.printf "COMBINE ..."; print_newline ();
    end;
  
  if not !no_combine then begin
      Comb_main.combine_func func;
      analysis func;
    end;

  if !one_debug && !debug_print_code
      then begin
          Printf.printf "Code after combine pass:";
          print_newline ();
          print_func func;
        end;
  
  Optimize4.remove_trywith func;

  if !one_debug && !debug_print_code
  then begin
      Printf.printf "Code after remove_trywith pass:";
          print_newline ();
          print_func func;
        end;

  Optimize4.jump2jump func;
  if !restart_analysis then one_analysis func;

  if !one_debug && !debug_print_code
  then begin
      Printf.printf "Code after jump2jump pass:";
          print_newline ();
          print_func func;
        end;

  if not !no_speculative_reloads then begin
      Optimize3.try_speculative_reloads := true;
      analysis func
    end;
  if func.fun_debug then
    Print_cfg.close_file !output func
    
let clean () =  
  Iter.reset ();
  Iter.add_node program.start_node;
  Iter.iter_nodes clean_node
        
let f sorted = 
  Peephole.initialize ();
  let old_debug_live = !debug_live in
  let old_debug_approx = !debug_approx in
  let old_debug_peephole = !debug_peephole in
  let old_debug_meet = !debug_meet in
  let old_debug_opt2 = !debug_opt2 in
  let old_debug_opt1 = !debug_opt1 in
  let old_debug_opt3 = !debug_opt3 in
  let old_debug_opt4 = !debug_opt4 in
  let old_debug_combine = !debug_combine in
  let old_debug_result = !debug_result in
  let rec iter list =
    match list with
      (level, globals) :: tail ->
        List.iter (fun global ->
            let func = Hashtbl.find program.desc global in
            if func.fun_debug then
              begin
                debug_combine := old_debug_combine;
                debug_live := old_debug_live;
                debug_approx := old_debug_approx;
                debug_peephole := old_debug_peephole;
                debug_meet := old_debug_meet;
                debug_opt2 := old_debug_opt2;
                debug_opt1 := old_debug_opt1;
                debug_opt3 := old_debug_opt3;
                debug_opt4 := old_debug_opt4;
                debug_result := old_debug_result;
                one_debug :=  (!debug_combine || !debug_live || !debug_approx
                    || !debug_opt1 || !debug_opt2|| !debug_opt3||
                  !debug_opt4 || !debug_peephole || !debug_meet
                    || !debug_result);
                
                if !debug_print_code && !one_debug
                then begin
                    Printf.printf "Code before pass:";
                    print_newline ();
                    print_func func;
                  end;
              
              end else begin
                debug_live := false;
                debug_approx := false;
                debug_peephole := false;
                debug_meet := false;
                debug_opt2 := false;
                debug_opt1 := false;
                debug_opt3 := false;
                debug_opt4 := false;
                debug_combine := false;
                debug_result := false;
                
                one_debug := false;
              end;
            
            let size = func.fun_size in
            analysis func;
            if !debug && func.fun_debug then
              (Printf.printf "Size %s : %4d -> %4d ***** %4d removed (%d)" func.fun_name size func.fun_size (size - func.fun_size) (
                  (int_of_float (
                      (((float_of_int func.fun_size) -. (float_of_int size))
                        /. (float_of_int size)) *. 100.))
                ); 
                print_newline ());
            
            (****
            Unused optimization: we compute the "header" of each function.
            The header is the part which only moves arguments to their
            final location (stack or register). At this point, this
            header can be moved to the caller side, since the caller
            spends some time putting good values in the registers.
  
            *)

            (*
            let instrs = func.code.instrs in
            let rec iter i =
              if i < Array.length instrs then
              match instrs.(i) with
                { opcode = Addl; 
                  args = [Const (Const_int _); Register Esp] } -> iter (i+1)
              | { opcode = Subl; 
                  args = [Const (Const_int _); Register Esp] } -> iter (i+1)
              | { opcode = Movl; 
                  args = [Register _; OffsetBase(Const_int _, Esp)] }
                -> iter (i+1)
              | { opcode = Movl; 
                  args = [Register _; Register _] } -> iter (i+1)
              | { opcode = Movl;
                  args = [OffsetBase(Const_int _, Esp); Register _ ] }
                -> iter (i+1)
              | _ -> 
                  if i > 0 then begin
                      Printf.printf "header %d" i;
                      print_newline ();
                      end
                    else ()
              else  begin
                  Printf.printf "header %d" i;
                  print_newline ();
                end
            in
            iter 0;
            *)
        ) globals;
        iter tail
    | _ -> ()
  in
  iter sorted;
  clean ()      
