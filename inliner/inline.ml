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

open Misc
open Asm
open Args
  
let stat_inline = ref 0
let stat_delete_frame = ref 0
let stat_noalloc = ref 0
  
let is_alloc s =
    if 
      (try Hashtbl.find program.desc s
        with Not_found -> Hashtbl.find program.env s).fun_alloc
    then
      raise Not_found
      
let modified = ref false
let rec allocp sorted =
  modified := false;
  let rec  iter list =
    match list with
      [] -> ()
    | (level, globals) :: tail ->
        List.iter (fun global -> 
            let func = Hashtbl.find program.desc global in
            begin
              try
                Iter.reset ();
                Iter.add_node func.code;
                Iter.iter_nodes (fun node ->
                    Array.iter (fun instr ->
                        match instr with
                          { opcode = Jmp; 
                            args = [ConstantBase (Const_symbol s)] } ->
                            if s<>global then is_alloc s
                        | { opcode = Jcond _;
                            args = [ConstantBase (Const_symbol s)] } ->
                            if s<>global then is_alloc s
                        | { opcode = Call _; directives = d }
                            when List.memq Dir_nogc d -> ()
                        | { opcode = Call (Some frame);
                            args = [ConstantBase (Const_symbol s)] } ->
                            if s<>global then is_alloc s
                        | { opcode = Call None;
                            args = [ConstantBase (Const_symbol s)] } ->
                            if s<>global then is_alloc s
                        | { opcode = Call _ } -> raise Not_found
                        | { opcode = Alloc _ } -> raise Not_found
                        | { opcode = Jmp; 
                            args = [ConstantBase(Const_label _)] } -> ()
                        | { opcode = Jmp } -> raise Not_found
                        | _ -> ()
                    ) node.instrs
                );              
                if func.fun_alloc then
                  begin
                    incr stat_noalloc; 
                    func.fun_alloc <- false;
                    modified := true;
                  end
              with Not_found -> ()
            end;
            Args.log (fun _ -> 
                Printf.sprintf "  %s: len = %d %s" 
                  global 
                  func.fun_size
                  (if func.fun_alloc then "" else "noalloc")
            );          
        ) globals;
        iter tail
  in
  iter sorted;
  if !modified then
    begin
      allocp sorted
    end

let add_inline_frames callerframe frame =
  match callerframe with
    None -> assert false
  | Some callerframe ->
      let new_frame = mkframe () in
      new_frame.size <- callerframe.size + frame.size - 4; (* return address *)
      new_frame.pos <- (List.map (fun pos -> pos - 4) callerframe.pos)
      @ frame.pos;
      new_frame
      
let copy_inline_instr callerframe instr =
  let instr = copy_instr instr in
  begin
    match instr with
      { opcode = Alloc (n_orig, n, frame, fast) } ->
        instr.opcode <- Alloc (n_orig, n, add_inline_frames callerframe frame, fast)
    | { opcode = Call (Some frame) } ->
        instr.opcode <- Call  (Some (add_inline_frames callerframe frame))
    | _ -> ()
  end;
  instr.dead <- true;
  instr
      
let copy_inline_instrs callerframe instrs = 
  Array.map (copy_inline_instr callerframe) instrs

let copy_inline_node callerframe node node2 = 
  let nodes = ref [] in
  let rec copy node =
    try
      List.assq node !nodes
    with Not_found ->
        let new_node = mknode () in
        nodes := (node, new_node) :: !nodes;
        new_node.instrs <- copy_inline_instrs callerframe node.instrs;
        let instr = copy_instr node.link in
        new_node.link <- instr;
        (match instr with
        (* Iter on all nodes of the function *)
            { opcode = Switch nodes } ->
              instr.opcode <- Switch (List.map copy nodes) 
          | { opcode = Setuptrap (node, next) } ->
              instr.opcode <- Setuptrap (copy node, copy next) 
          | { opcode = Jmp; 
              args = [ConstantBase (Const_label node)] } ->
              instr.args <- [ConstantBase (Const_label (copy node))]
          | { opcode = Jcond(comp, next); 
              args = [ConstantBase (Const_label node)] } ->
              instr.opcode <- Jcond (comp, copy next);
              instr.args <- [ConstantBase (Const_label (copy node))]
          | { opcode = Poptrap next } ->
              instr.opcode <- Poptrap (copy next)
              (* Redirect leaves to node2 *)
          | { opcode = Jmp;
              args = [ConstantBase (Const_symbol s)]; } ->
              new_node.link <- mkinstr Jmp [
                ConstantBase (Const_label node2)] [];
              (* OK: for now, we simply put the callerframe when we replace 
              a tailcall by a call before inlining. We must also take care 
              of the result directive. Here, we assume that only caml
              functions can be called in a terminal form. Thus, the
              directive is Dir_res [Eax]. *)
              new_node.instrs <- Array.concat 
              [ new_node.instrs; [| 
              mkinstr (Call (copy_callframe callerframe)) 
                  [ConstantBase (Const_symbol s)] 
                    ((Dir_res [Register Eax]) ::instr.directives) |] ]
          | { opcode = Jmp; args = args; directives = directives } ->
              new_node.link <- mkinstr Jmp [
                ConstantBase (Const_label node2)] [];
              (* OK: for now, we simply put the callerframe when we replace 
              a tailcall by a call before inlining. We must also take care 
              of the result directive. Here, we assume that only caml
              functions can be called in a terminal form. Thus, the
              directive is Dir_res [Eax]. *)
              new_node.instrs <- Array.concat 
                [ new_node.instrs; [| 
                  mkinstr (Call (copy_callframe callerframe)) args
                    ((Dir_res [Register Eax]) :: directives) |] ]
          | { opcode = Ret } ->
              new_node.link <- mkinstr Jmp [
                ConstantBase (Const_label node2)] []
          | _ -> ()
        );
        new_node
  in
  copy node
  
let inline_fun callerframe node1 pos func =
  assert (func.arity <= nparams);
  let node2 = cut_instr node1 pos in
  let inlined = copy_inline_node callerframe func.code node2 in
  node1.link <- mkinstr Jmp [ConstantBase (Const_label inlined)] [];
  ()

  (* Inlining of obj_dup ... *)
let inline_obj_dup frame node1 i arg =
  let node2 = cut_instr node1 i in
  node2.instrs <- Array.concat [
    [|
      mkinstr Movl [arg; Register Eax] [];
      mkinstr Movl [Register Eax; Register Esi] [];
      mkinstr Movl [OffsetBase(Const_int (-4), Eax); Register Ecx] [];
      mkinstr Movl [Register Ecx; Register Edx] [];
      mkinstr Shrl [Const(Const_int 10); Register Ecx] [];
      mkinstr Leal [OffsetIndexScale(Const_int 4, Ecx, 4); Register Eax] [];
      mkinstr (Call 
          (Some { size = frame.size - 4;
            pos = ( (* esi *)((4 lsl 1) + 1) ::
                
                (List.map (fun pos -> pos - 4) frame.pos)) }
        )) [ConstantBase(Const_symbol "caml_alloc")]
        [Dir_res [Register Eax]; Dir_args [Register Eax];
        Dir_live (
          (Register Esi) :: (Register Edx) :: 
          (List.map (fun pos -> OffsetBase(Const_int (pos-4), Esp)))
          frame.pos)];
      mkinstr Movl [Register Eax; Register Edi] [];
      mkinstr Andl [Const(Const_int 0xffffcff); Register Edx] [];
      mkinstr Movl [Register Edx; OffsetBase(Const_int (-4), Edi)] [];
      mkinstr (Rep Movsl) [Register Esi; Register Edi; Register Ecx] [];
    |];
    Array.sub node2.instrs 3 (Array.length node2.instrs - 3)];
  node1.link <- mkinstr Jmp [ConstantBase (Const_label node2)] [];
  ()
  
(*
  (* Inlining of obj_dup ... *)
let inline_array_copy frame node1 i =
  let frame = match frame with None -> assert false | Some frame -> frame in
  let node2 = cut_instr node1 i in
  node2.instrs <- Array.concat [
    [|
      mkinstr Movl [Register Eax; Register Esi] [];
      mkinstr Movl [OffsetBase(Const_int (-4), Eax); Register Ecx] [];
      mkinstr Cmpi ON_BYTE)
      mkinstr Movl [Register Ecx; Register Edx] [];
      mkinstr Shrl [Const(Const_int 10); Register Ecx] [];
      mkinstr Leal [OffsetIndexScale(Const_int 4, Ecx, 4); Register Eax] [];
      mkinstr (Call 
          (Some { size = frame.size - 4;
            pos = ( (* esi *)((4 lsl 1) + 1) ::
                
                (List.map (fun pos -> pos - 4) frame.pos)) }
        )) [ConstantBase(Const_symbol "caml_alloc")]
        [Dir_res [Register Eax]; Dir_args [Register Eax];
        Dir_live (
          (Register Esi) :: (Register Edx) :: 
          (List.map (fun pos -> OffsetBase(Const_int (pos-4), Esp)))
          frame.pos)];
      mkinstr Movl [Register Eax; Register Edi] [];
      mkinstr Andl [Const(Const_int 0xffffcff); Register Edx] [];
      mkinstr Movl [Register Edx; OffsetBase(Const_int (-4), Edi)] [];
      mkinstr (Rep Movsl) [Register Esi; Register Edi; Register Ecx] [];
    |];
    Array.sub node2.instrs 3 (Array.length node2.instrs - 3)];
  node1.link <- mkinstr Jmp [ConstantBase (Const_label node2)] [];
  ()
*)
  
  
let rec iter_inline i node =
  let len = Array.length node.instrs in
  if len>i then
    let instr = node.instrs.(i) in
    if not instr.dead then
      match instr with
      | { opcode = Call frame;
          args = [ConstantBase (Const_symbol s)] } ->
(*          if String.sub s 0 10 = "Array_copy" then begin
              incr stat_inline;
              inline_array_copy frame node1 i;
              iter_inline i node
          end else *)
          begin
              try
              let func = try Hashtbl.find program.desc s with
                  Not_found -> Hashtbl.find program.env s
              in                  
              let len = func.fun_size in
              if (not !no_inline) &&
                (len <= !inlining || func.fun_inline) &&
                func.arity <= nparams
              then
                begin
                  incr stat_inline;
                  Args.log (fun _ -> Printf.sprintf "   (Inlining %s)" s);
                  inline_fun frame node i func;
                  iter_inline i node
                end
              else
                begin
                  match frame with
                    None -> iter_inline (i+1) node
                  | Some frame ->
                      if not func.fun_alloc then
                        begin
                          incr stat_delete_frame;
                          instr.opcode <- Call None;
                        end;
                      iter_inline (i+1) node
                end
            with Not_found -> iter_inline (i+1) node
          end
      
      | { opcode = Pushl; args = [arg] } when len>i+3 &&
        (match node.instrs.(i+1) with
            { opcode = Movl; 
              args = [Const (Const_symbol "obj_dup"); Register Eax] } -> true
          | _ -> false) ->
          begin
            match node.instrs.(i+2), node.instrs.(i+3) with
              { opcode = Call (Some frame); 
                args = [ConstantBase(Const_symbol "caml_c_call")] },
              { opcode = Addl; args = [Const(Const_int 4); Register Esp] } ->
                inline_obj_dup frame node i arg;
                incr stat_inline;
                iter_inline (i+1) node
                
            | _ -> 
                Printf.printf "FAIL TO MATCH obj_dup"; print_newline ();
                iter_inline (i+1) node
                
                
          end
          
          
      | _ -> iter_inline (i+1) node
    else
      begin
        instr.dead <- false;
        iter_inline (i+1) node
      end

let rec iter_instrs node instrs =
  match instrs with
  | [] -> []
  | instr :: instrs ->
      instr :: (iter_instrs node instrs)
      
let special_inline node =
  let instrs = Array.of_list (iter_instrs node
      (Array.to_list node.instrs)) in
  node.instrs <- instrs;
  iter_inline 0 node
      
let inline level =
  if not !no_inline then
    List.iter (fun global ->
        let func = Hashtbl.find program.desc global in
        Iter.reset ();
        Iter.add_node func.code;
        Iter.iter_nodes special_inline;
  ) program.globals
  
      (*
      match node.link with
      
      
      | { opcode = Jmp;
          args = [ConstantBase (Const_symbol s)] } ->
          incr nnodes;
          begin
            try
              let func = try Hashtbl.find program.desc s with
                  Not_found -> Hashtbl.find program.env s
              in
              let len = Array.length  func.code.instrs in
              if not func.fun_alloc && 
                len <= level &&
                func.code.link.opcode = Ret &&
                func.arity < 8
              then
                begin
                  incr stat_inline;
                  Po.replace_instr node i func.code.instrs;
                  node.link <- Ret;
                  iter i
                end
              else
                iter (i+1)
            with Not_found -> iter (i+1)
          end
      | _ -> ()
  )
      *)
      

      