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
open Misc
open Approx  
  
    
let printf a =
  if !debug_peephole then 
    Printf.printf a
  else
  let rec f = (fun _ -> Obj.magic f) in Obj.magic f

let print_newline () = 
  if !debug_peephole then 
    Pervasives.print_newline ()
    
let stat_peepholes = ref 0
  
let dead_after instr reg =
  try
    let nr = regindex_all reg in
    instr.leave_instr.liveness <> [||] &&
    instr.leave_instr.liveness.(nr) = []
  with _ -> false
      
      (* 
      We should have some dynlink mecanism to load peephole 
      optimizations for a given file inside the runtime, or some
      language to express these optimizations.
      *)

let var_directives = Match.mkvar (ref [])
let var_used_by = Match.mkvar (ref [])
let var_use = Match.mkvar (ref [])
let var_instr_num = Match.mkvar (ref 0)
let var_enter_instr = Match.mkvar (ref noapprox)
let var_leave_instr = Match.mkvar (ref noapprox)
let var_dead = Match.mkvar (ref false)
let var_frame = Match.mkvar (ref None)
  
let mkinstr_matcher op args enter_instr leave_instr = { 
    opcode = (match op with
        Call _ -> Call var_frame
      | _ -> op
    ); 
    args = args; 
    directives = var_directives;
    used_by = var_used_by;
    use = var_use;
    instr_num = var_instr_num;
    enter_instr = (if enter_instr = noapprox then var_enter_instr else
        enter_instr);
    leave_instr = (if leave_instr = noapprox then var_leave_instr else
        leave_instr);
    dead = var_dead;
  }
        
let peepholes = ref []
let initialize _ =
  peepholes := List.map (fun (name, vars, orig, preds, dest) ->
      printf "PEEPHOLE"; print_newline ();
      let tailr = ref [] in
      let tail = Match.mkvar tailr in
      (List.map (fun { opcode = opcode; args = args; enter_instr = enter_instr;
              leave_instr = leave_instr; } ->
            mkinstr_matcher opcode args enter_instr leave_instr
        )
        (Array.to_list orig.instrs)) @ tail,
      preds,
      (Array.to_list dest.instrs) @ tail,
      tailr,
      (Array.to_list orig.instrs),
      (Array.to_list dest.instrs),
      name
  ) !Macros.peepholes

let dead state reg = 
  try
    let r = regindex_all reg in
    if not (Array.length state.liveness > r && state.liveness.(r) = [])
    then (false)
    else true
  with _ -> 
      false

let rec simplify_const c =
  match c with
    Const_add (c1,c2) -> begin
        let c1 = simplify_const c1 in
        let c2 = simplify_const c2 in
        match c1, c2 with
          Const_int n1, Const_int n2 -> Const_int (n1+n2)
        | Const_add (Const_int n1, c2), Const_int n2 ->
            simplify_const (Const_add (Const_int (n1+n2), c2))
        | Const_int n2, Const_add (Const_int n1, c2) ->
            simplify_const (Const_add (Const_int (n1+n2), c2))
        | Const_add (Const_int n1, c1), Const_add (Const_int n2, c2) ->
            simplify_const (Const_add (Const_int (n1+n2), Const_add (c1,c2)))
        | Const_add (Const_int n1, c1), c2 ->
            simplify_const (Const_add (Const_int n1, Const_add(c1,c2)))
        | _ -> Const_add (c1,c2)
      end
  | Const_sub (c1,c2) ->  
      begin
        let c1 = simplify_const c1 in
        let c2 = simplify_const c2 in
        match c1, c2 with
          Const_int n1, Const_int n2 -> Const_int (n1-n2)
        | _ -> Const_sub (c1,c2)
      end
  | Const_mul (c1,c2) ->  
      begin
        let c1 = simplify_const c1 in
        let c2 = simplify_const c2 in
        match c1, c2 with
          Const_int n1, Const_int n2 -> Const_int (n1*n2)
        | _ -> Const_mul (c1,c2)
      end
  | _ -> c
      
let rec distinct regs =
  match regs with
    r :: rs -> List.for_all ((<>) r) rs && distinct rs
  | [] -> true

let value state r =
  let r = regindex r in
  if r < Array.length state.registers then
    state.registers.(r) else raise Exit
      
let predicat pred =
(* Unref all matching variable in the predicats *)
  let pred = Match.copy pred in
  match pred with
    SameReg (r::l) -> List.for_all ( (=) r) l
  | Dead (state, regs) -> List.for_all (dead state) regs
  | Equal (n :: l) ->   
      let l = List.map simplify_const l in
      let n = simplify_const n in
      List.for_all ( (=) n) l
  | Distinct l -> 
      let l = List.map bits32 l in
      distinct l
  | SameValue ((state, r) :: vs) ->
      (try
          let v = value state r in
          List.for_all (fun (state, r) -> v = value state r) vs
        with Exit -> false)
  | Instr (op, l) -> List.mem op l
  | _ -> false
      
let rec do_peephole node instrs =
  try
    let instrs = 
      match instrs with
      | [] -> []
      | instr :: _ ->
          List.fold_left (fun instrs (orig, preds, dest,tail, pattern, replace, name) ->
              if Match.compare instrs orig then begin
                  if !debug_peephole then begin
                      printf "Good match for %s" name;
                      print_newline ();
                    end;
                  if List.for_all predicat preds then begin
                      if !debug_peephole then begin
                          printf "PATTERN(%s) = " name; print_newline ();
                          List.iter (fun i ->
                              printf "%s" (SimplePrint.string_of_instr i);
                              print_newline ();
                          ) (Match.copy pattern);
                        end;
                      let copy = Match.copy dest in
                      if !debug_peephole then begin
                          printf "NEW = "; print_newline ();
                          List.iter (fun i ->
                              printf "%s" (SimplePrint.string_of_instr i);
                              print_newline ();
                          ) (Match.copy replace);
                        end;
                      Approx.restart_analysis := true;
                      incr stat_peepholes;
                      copy
                    end
                  else  begin
                      if !debug_peephole then begin
                          printf "Not OK:";
                          List.iter (fun pred -> 
                              if not (predicat pred) then
                                printf "%s " (match pred with
                                    SameReg _ -> "SameReg"
                                  | Dead _ ->  "Dead"
                                  | Equal _ ->   "Equal"
                                  | Distinct _ -> "Distinct"
                                  | SameValue _ -> "SameValue"
                                  | Instr _ -> "Instr"
                                )) preds;
                          print_newline ();
                        end;
                      instrs
                    end
                end
              else instrs
          ) instrs !peepholes in
    match instrs with 
    (*
    ({ opcode = Movzbl; 
        args = [OffsetBase(Const_int n1, r1); Register r2] } as i1) ::
    { opcode = Sall;   args = [Const(Const_int 9); Register r3] } ::
    { opcode = Incl;   args = [Register r4] } ::
    { opcode = Movzbl; args = [OffsetBase(Const_int n2, r5); Register r6] } ::
    { opcode = Lea;    
      args = [OffsetIndexScale(Const_int 1, r7, 2); Register r8] } ::
    ({ opcode = Orl;    args = [Register r9; Register r10] } as i6) :: 
    tail when
    r1 = r5 && (* BUFFER *)
    r2 = r3 && r3 = r4 && r4 = r9 && (* HIGH BYTE *)
    r6 = r7 && r7 = r8 && r8 = r10 && (* LOW BYTE *)
    n1 = n2 + 1 &&
    dead_after i6 r9 ->
      do_peephole (
        (mkinstr Movzwl [OffsetBase(Const_int n2, r1); Register r10] []) ::
        (mkinstr Lea [OffsetIndexScale(Const_int 1, r10, 2); Register r10] []) ::
    tail)
    | [ { opcode = Call _; args = [ConstantBase(Const_symbol s)] };
        { opcode = Cmpl; args = [Const(Const_int n); Register Eax] } ] ->
        if String.sub s 0 15 = "List_length_aux" && n = 1 then
          match node.link with
            { opcode = Jcond (Isigned Cle, next) } ->
              restart_analysis := true;
              incr stat_peepholes;
              printf "Found List_length_aux";
              print_newline ();
              node.link.opcode <- Jcond (Isigned Cne, next);
              [ mkinstr Testl [Const(Const_int 1); Register Ebx] [] ]
          | _ -> instrs
        else instrs
    *)      
       (* OK, if you want to use List.length to verify that a list has
      at least one or two elements, we can ensure that it will not be too 
      expensive.
      We can check unexpensively:
      List.length l = 0
      List.length l = 1
      List.length l > 0
      List.length l > 1
      List.length  < 2
      List.length  <> 1
  *)

    | [ { opcode = Call _; args = [ConstantBase(Const_symbol s)] };
        { opcode = Cmpl; args = [Const(Const_int n); Register Eax] } ] ->
        if String.sub s 0 15 = "List_length_aux" then
          match node.link with
            { opcode = Jcond (comp, next); 
              args = [ConstantBase(Const_label other)];
              leave_instr = leave_instr
            } when
            leave_instr.liveness <> [||] &&
            leave_instr.liveness.(0) = [] 
            ->
              begin
                match comp, n with
          (* len = 0: jne ou jg, 1 *)
                | (Isigned Cne | Iunsigned Cne | Isigned Cgt), 1
                | (Isigned Cge), 3 ->
                    incr stat_peepholes;
                    restart_analysis := true;
                    printf "Found List.length <> 0"; print_newline ();
                    node.link.opcode <- Jcond (Isigned Cne, next);
                    [ mkinstr Cmpl [Const(Const_int 1); Register Ebx] [] ]
                | (Isigned Cle | Isigned Ceq | Iunsigned Ceq), 1
                | (Isigned Clt), 3 ->
                    incr stat_peepholes;
                    restart_analysis := true;
                    printf "Found List.length = 0"; print_newline ();
                    node.link.opcode <- Jcond (Isigned Ceq, next);
                    [ mkinstr Cmpl [Const(Const_int 1); Register Ebx] [] ]
                | (Isigned Cne | Iunsigned Cne), 3 ->
                    incr stat_peepholes;
                    restart_analysis := true;
                    printf "Found List.length <> 1"; print_newline ();
                    let new_node = mknode () in
                    (* if v = [] then other else new_node *)
                    node.link <- mkinstr (Jcond (Isigned Ceq, new_node)) 
                    [ConstantBase(Const_label other)] [];
                    new_node.instrs <- [|
                      mkinstr Movl [OffsetBase(Const_int 4, Ebx); Register Eax] [];
                      mkinstr Cmpl [Const(Const_int 1); Register Eax] []
                    |];
                    (* if *v = [] then next else other *)
                    new_node.link <- mkinstr (Jcond (Isigned Cne, next)) 
                    [ConstantBase(Const_label other)] [];
                    [ mkinstr Cmpl [Const(Const_int 1); Register Ebx] []]
                | (Isigned Ceq | Iunsigned Ceq), 3 ->
                    incr stat_peepholes;
                    restart_analysis := true;
                    printf "Found List.length = 1"; print_newline ();
                    let new_node = mknode () in
                    (* if v = [] then next else new_node *)
                    node.link <- mkinstr (Jcond (Isigned Ceq, new_node)) 
                    [ConstantBase(Const_label next)] [];
                    new_node.instrs <- [|
                      mkinstr Movl [OffsetBase(Const_int 4, Ebx); Register Eax] [];
                      mkinstr Cmpl [Const(Const_int 1); Register Eax] []
                    |];
                    (* if *v = [] then other else next *)
                    new_node.link <- mkinstr (Jcond (Isigned Ceq, next)) 
                    [ConstantBase(Const_label other)] [];
                    [ mkinstr Cmpl [Const(Const_int 1); Register Ebx] []]
                | Isigned Cgt, 3
                | Isigned Cge, 5
                  ->
                    incr stat_peepholes;
                    restart_analysis := true;
                    printf "Found List.length > 1"; print_newline ();
                    let new_node = mknode () in
                    (* if v = [] then next else new_node *)
                    node.link <- mkinstr (Jcond (Isigned Ceq, new_node)) 
                    [ConstantBase(Const_label next)] [];
                    new_node.instrs <- [|
                      mkinstr Movl [OffsetBase(Const_int 4, Ebx); Register Eax] [];
                      mkinstr Cmpl [Const(Const_int 1); Register Eax] []
                    |];
                    (* if *v = [] then next else other *)
                    new_node.link <- mkinstr (Jcond (Isigned Cne, next)) 
                    [ConstantBase(Const_label other)] [];
                    [ mkinstr Cmpl [Const(Const_int 1); Register Ebx] []]
                | Isigned Clt, 5 (* < 2 *)
                | Isigned Cle, 3 (* <= 1 *)
                  ->
                    incr stat_peepholes;
                    restart_analysis := true;
                    printf "Found List.length < 2"; print_newline ();
                    let new_node = mknode () in
                    (* if v = [] then other else new_node *)
                    node.link <- mkinstr (Jcond (Isigned Ceq, new_node)) 
                    [ConstantBase(Const_label other)] [];
                    new_node.instrs <- [|
                      mkinstr Movl [OffsetBase(Const_int 4, Ebx); Register Eax] [];
                      mkinstr Cmpl [Const(Const_int 1); Register Eax] []
                    |];
                    (* if *v = [] then other else next *)
                    new_node.link <- mkinstr (Jcond (Isigned Cne, next)) 
                    [ConstantBase(Const_label other)] [];
                    [ mkinstr Cmpl [Const(Const_int 1); Register Ebx] []]
                | _ -> instrs
              end
          | _ -> instrs
        else instrs
    | _ -> instrs
  with _ -> instrs      
      
let rec iter_instrs node instrs =
  let instrs = do_peephole node instrs in
  match instrs with
    [] -> []
  | instr :: tail -> instr :: (iter_instrs node tail)

let print_node node =
  printf "NODE %d" node.node_ident.label;
  print_newline ();
  Array.iteri (fun i ins->
      printf "Instr %3d(%6d) : %s" i ins.instr_num (SimplePrint.string_of_instr ins);
      print_newline ();)
  node.instrs
      
let iter_node node =  
  let instrs = Array.to_list node.instrs in
  node.instrs <- Array.of_list (List.map copy_instr (iter_instrs node instrs))
