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

(* These are the speculative optimizations. There are only done once to
ensure the termination of the process *)

open Args
open Asm
  

let try_speculative_reloads = ref false
let stat_speculative_reloads = ref 0

let spills = ref []
let find_spills node =
  Array.iter (fun instr ->
      match instr with
        { opcode = Movl; args = [v; OffsetBase(Const_int n, Esp)] } ->
          (* spill a value in the stack *)
          let nused = List.length instr.used_by in
          if instr.used_by = [] then
            (Printf.printf "unused instr"; print_newline ())
          else
            spills := (nused, instr) :: !spills
      | _ -> ()
  ) node.instrs

let availables = ref []
let speculative_reloads = ref []  
  
let sort_spills () =
  let sorted = Sort.list (fun (n1,_) (n2,_) -> n1 >= n2) !spills in
  let rec iter list =
    match list with
      [] -> ()
    | (n, instr) :: tail ->
        if n > 0 then
          begin
            (*
            Printf.printf "Testing %s (%d)" (SimplePrint.string_of_instr instr) n;
            print_newline ();
          *)
            let vs = instr.leave_instr.registers in
            let liveness = instr.leave_instr.liveness in
            if vs <> [||] && liveness <> [||] then
              let ndeads = ref 0 in
              for i = 0 to nregs - 1 do
                if liveness.(i) = [] then incr ndeads
              done;
              
              (*  if !ndeads > 0 then   
                (Printf.printf "%d deads" !ndeads; print_newline ());
              *)
              
              let counts = Array.create nregs 0 in
              List.iter (fun instr2 ->
                  (* Printf.printf "1"; print_newline (); *)
                  let regs = instr2.enter_instr.registers in
                  if (regs <> [||]) 
                    && instr2.opcode = Movl &&
                    (match instr2.use with
                        [i] when i == instr -> true
                      | _ -> 
                          (*
                          Printf.printf "different sources:";
                          print_newline ();
                          List.iter (fun i ->
                              print_instr i;
                          ) instr2.use;
                      *)
                          false) then
                    for i = 0 to nregs -1 do
                      if regs.(i) = vs.(i) then
                        (* begin
                        Printf.printf "same value"; print_newline (); *)
                          if liveness.(i) = [] then                            
                            counts.(i) <- counts.(i) + 1
(*                          else
                            begin
                              Printf.printf "not dead"; print_newline ();
                            end
                        end else 
                        (Printf.printf "-"; print_newline ()); *)
                    done
              ) instr.used_by;
              let navail = ref 0 in
              for i = 0 to nregs - 1 do
                let c = counts.(i) in
                if c = n then incr navail;
              done;
              for i = 0 to nregs - 1 do
                let c = counts.(i) in
                if c = n then begin
                    (*
                    Printf.printf "%s is %d/%d available for %s" 
                    (string_of_register regs.(i)) c n 
                    (SimplePrint.string_of_instr instr);
                    print_newline ();
                *)
                    availables := (c, !navail, i, vs.(i), instr) :: !availables
                  end else
                if c >= n/2+3 then
                  let navail = ref 1 in
                  for i = i+1 to nregs - 1 do
                    let cc = counts.(i) in
                    if cc = c then incr navail;
                  done;
                  (*
                  Printf.printf "%s is %d/%d available for %s" 
                  (string_of_register regs.(i)) c n 
                  (SimplePrint.string_of_instr instr);
                  print_newline ();
                  *)
                  availables := (c, !navail, i, vs.(i), instr) :: !availables
              done;
          end;
        iter tail
  in
  availables := [];
  iter sorted;
  let sorted = Sort.list (fun (c1, n1, _, _ ,_) (c2, n2, _,_,_) -> c1 > c2 ||
        (c1 = c2) && (n1 <= n2)) !availables in
  let instrs = ref [] in
  let regs_used = ref [] in
  let rec iter list = match list with
      [] -> ()
    | (_, _, i, v, instr) :: tail ->
        if not (List.mem instr.instr_num !instrs || List.mem (i,v) !regs_used)
        then
          begin
            match instr with
              { args = [src; dst]; } ->
                (*   Printf.printf "Speculative reload"; print_newline (); *)
                speculative_reloads := (instr, [src;Register regs.(i)] ) :: !speculative_reloads;
                regs_used := (i,v) :: !regs_used;
                instrs := instr.instr_num :: !instrs;
            | _ -> assert false
          end;
        iter tail
  in
  iter sorted

let insert_reloads node =
  node.instrs <- Array.of_list (
    let rec iter list = match list with
        [] -> []
      | instr :: instrs ->
          try
            let args = List.assq instr !speculative_reloads in
            incr stat_speculative_reloads;
            Approx.restart_analysis := true;
            instr :: (mkinstr Movl args []) :: (iter instrs)
          with
            Not_found -> instr :: (iter instrs)
    in
    iter (Array.to_list node.instrs));
  speculative_reloads := []
    
let func func = 
  
  if !debug_opt3 then begin
      Printf.printf "****************************** Optimizing(3) %s" func.fun_name;
      print_newline ();
    end;
  
  spills := [];
  availables := [];
  speculative_reloads := [];
  
  Iter.reset ();
  Iter.add_node func.code;
  Iter.iter_nodes find_spills;
  sort_spills ();
  Iter.reset ();
  Iter.add_node func.code;
  Iter.iter_nodes insert_reloads;
  
  try_speculative_reloads := false