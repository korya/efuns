(***********************************************************************)
(*                                                                     *)
(*                           AsmOpt                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Args
open Asm

type cmx_infos = {
    ui_name: string;                    (* Name of unit implemented *)
    ui_imports_cmi: (string * Obj.t) list; (* Interfaces imported *)
    ui_imports_cmx: (string * Obj.t) list; (* Infos imported *)
    (* NOT INTERESTING 
    mutable ui_approx: value_approximation;     (* Approx of the structure *)
    mutable ui_curry_fun: int list;             (* Currying functions needed *)
    mutable ui_apply_fun: int list;             (* Apply functions needed *)
    mutable ui_force_link: bool }               (* Always linked *)
    *)
  }

let clean_instr instr =
  instr.used_by <- [];
  instr.use <- [];
  instr.enter_instr <- noapprox;
  instr.leave_instr <- noapprox
  
let clean_node node =
  Array.iter clean_instr node.instrs;
  clean_instr node.link;
  node.back_edges <- [];
  node.enter_node <- noapprox;
  node.leave_node <- noapprox;
  node.node_names <- []
  
let clean_program () =
  Hashtbl.clear program.env;
  List.iter (fun (_, vars, _, _, _ ) ->
      List.iter (fun s -> s := noapprox) vars.match_states;
  ) program.peepholes;
  Iter.reset ();
  Iter.add_node program.start_node;
  Iter.iter_nodes clean_node;
  Hashtbl.clear program.env;
  program.start_node <- nonode
  
let stat_asx_loaded = ref 0
let stat_initial_size = ref 0
let stat_final_size = ref 0
let stat_max_size = ref 0
  
let lexfile lexbuf =
(*   Printf.printf "pos %d" (Lexing.lexeme_start lexbuf); print_newline (); *)
  Archlexer.lexfile lexbuf

let cmx_magic_number = "Caml1999Y006"

let read_cmx filename =
  let ic = open_in_bin filename in
  try
    let buffer = String.create (String.length cmx_magic_number) in
    really_input ic buffer 0 (String.length cmx_magic_number);
    if buffer <> cmx_magic_number then begin
        close_in ic;
        raise Not_found
      end;
    let ui = (input_value ic : cmx_infos) in
    close_in ic;
    ui
  with End_of_file | Failure _ ->
      close_in ic;
      raise Not_found
      
let optimize file =
  let base = Filename.chop_extension file in    
  
  log (fun _ -> Printf.sprintf "Optimize %s" file);
  if !debug then 
    (Printf.printf "+ Reading assembler file";
      print_newline ());
  let ic = open_in file in
  Asm.reset_parser ();
  let _ = Match.clear () in
  Macros.peepholes := [];
  Peephole.peepholes := [];
  let lexbuf = Lexing.from_channel ic in
  let _ = 
    try
      Archparser.asmfile Archlexer.lexfile lexbuf
    with
      s -> 
        Printf.printf "Exception at file %s, pos %d" file (
          Lexing.lexeme_start lexbuf);
        print_newline ();
        raise s
  in
  close_in ic;
  if !Match.count <> 0 then
    failwith (Printf.sprintf "%d Macro variables in assembly file" 
        !Match.count);
  
  program.peepholes <- [];
  if !Args.debug_all_globals then debug_symbols := program.globals;
  
  List.iter (fun name -> 
      let func = Hashtbl.find program.desc name in
      func.fun_debug <- true) !debug_symbols;
  
  if !debug then 
    (Printf.printf "+ Reading macro file";
      print_newline ());
  let macros = 
    let file = Printf.sprintf "%s.%s" base Asm.arch in
    try
      let file = Utils.find_in_path path file in
      let ic = open_in file in
      let lexbuf = Lexing.from_channel ic in
      let macros = 
        try
          let _ = Match.clear () in
          Archparser.macrofile Archlexer.lexfile lexbuf
        with
          s -> Printf.printf "Exception at file %s, pos %d" file (
              Lexing.lexeme_start lexbuf);
            print_newline ();
            raise s
      in
      close_in ic;
      
      if !verbose then (
          Printf.printf "Macro file %s loaded" file;
          print_newline ());
      macros
    with _ ->  []
  
  in
  
  
  let _ = 
    let file = Printf.sprintf "asmopt.%s" Asm.arch in
    try
      let file = Utils.find_in_path path file in
      if !debug then 
        (Printf.printf "+ Reading common macro file";
          print_newline ());
      let ic = open_in file in
      let lexbuf = Lexing.from_channel ic in
      let macros = 
        try
          let _ = Match.clear () in
          Archparser.macrofile Archlexer.lexfile lexbuf
        with
          s -> Printf.printf "Exception at file %s, pos %d" file (
              Lexing.lexeme_start lexbuf);
            print_newline ();
            raise s
      in
      close_in ic;
      Macros.apply macros program;
      program.peepholes <- [];
    with _ -> ()
  in
      
  (* This should not be done: I shoud simply modify the emit.mlp file
  of ocamlopt to add a directive in the assembler file listing the
  dependencies of the file. *)
  
  if !debug then 
    (Printf.printf "+ Reading cmx file";
      print_newline ());
  let cmx = 
    let file = Printf.sprintf "%s.cmx" base in
    if Sys.file_exists file then
      try
        let unit = read_cmx file in
        if !verbose then (
            Printf.printf "%s description loaded" file;
            print_newline ());
        List.map fst unit.ui_imports_cmx
      with _ -> 
          Printf.printf "Error while loading %s" file;
          print_newline ();[]
    else [] in
  
  if !debug then 
    (Printf.printf "+ Reading asx files";
      print_newline ());
  
  let rec load_asx modules =
    match modules with
      [] -> ()
    | m :: tail ->
        if List.mem m program.depends then
          load_asx tail
        else
        let file = Printf.sprintf "%s.asx" (String.uncapitalize m) in
        try
          let file = Utils.find_in_path path file in
          let ic = open_in_bin file in
          let asx = input_value ic in
          close_in ic;
          if !debug then (
              Printf.printf "    (Loaded %s)" file;
              print_newline ());
          incr stat_asx_loaded;
          List.iter (fun name ->
              let func = Hashtbl.find asx.desc name in
              Hashtbl.add program.env name func;              
              Iter.iterq_nodes func.code (fun node ->
                  node.node_ident <- mkident ());
          ) asx.globals;
          Macros.peepholes := asx.peepholes @ !Macros.peepholes;
          program.depends <- m :: program.depends;
          load_asx (asx.depends @ tail)
        with Not_found -> 
            load_asx tail
  in            
  
  load_asx cmx;
  
  if !debug then 
    (Printf.printf "+ Generating initial control flow graph";
      print_newline ());
  Print_cfg.print_globals base;
  
  begin
    try
      if not !noopt then 
        begin
          if !debug then 
            (Printf.printf "+ Applying macros";
              print_newline ());
          Macros.apply macros program;

          (* Apply non-standard macros first *)
          if !debug then 
            (Printf.printf "Loaded %d peepholes descriptions" 
                (List.length !Peephole.peepholes);
              print_newline ());
          program.peepholes <- List.rev program.peepholes;
          
          if !debug then 
            (Printf.printf "+ Sorting functions";
              print_newline ());
          
          let sorted = Toposort.sort program in
    (*
    List.iter (fun (level,list) ->
        Printf.printf "Level %d:" level;
        print_newline ();
        List.iter (fun name -> 
            Printf.printf "%s" name;
            print_newline ();
            ) list        
    ) n;
    *)
          if !debug then 
            (Printf.printf "+ Creating start node";
              print_newline ());
          Cfg.start ();
          
          if !debug then 
            (Printf.printf "+ Merging blocks";
              print_newline ());
          Cfg.make_merge_blocks ();
          
          if !debug then 
            (Printf.printf "+ Simplifying switches";
              print_newline ());
          Po.simplify ();
          
          if !debug then 
            (Printf.printf "+ Merging blocks";
              print_newline ());
          Cfg.make_merge_blocks ();
          
          if !debug then 
            (Printf.printf "+ Searching opened functions";
              print_newline ());
          Cfg.close_functions ();
          
          List.iter (fun global ->
              let func = Hashtbl.find program.desc global in
              stat_initial_size := !stat_initial_size + func.fun_size) 
          program.globals;
          
          if !debug then 
            (Printf.printf "+ Searching static functions";
              print_newline ());
          Inline.allocp sorted;
          
          if !debug_inline then 
            (Printf.printf "+ Generating control flow graph before inlining";
              print_newline ();
              Print_cfg.print_globals (base ^ "-inline"));
          
          if !debug then 
            (Printf.printf "+ Inlining functions";
              print_newline ());  
          Inline.inline !inlining;
          
          
          if !debug then 
            (Printf.printf "+ Merging blocks";
              print_newline ());
          Cfg.make_merge_blocks ();
          
          
          if !debug then 
            (Printf.printf "+ Searching opened functions";
              print_newline ());
          Cfg.close_functions ();
          
          List.iter (fun global ->
              let func = Hashtbl.find program.desc global in
              stat_max_size := !stat_max_size + func.fun_size) 
          program.globals;
          
          if !debug_inline then 
            (Printf.printf "+ Generating control flow graph after inlining";
              print_newline ();
              Print_cfg.print_globals (base ^ "+inline"));
      (*
      if not !noapprox then (
          if !debug then 
            (Printf.printf "+ Computing approximations";
              print_newline ());
          Approx.compute_approximations sorted);  
      
      if !debug then
        (Printf.printf "+ Liveness optimisation";
          print_newline ());  
      Liveness.liveness ();
      *)
(*      
      if !debug then 
        (Printf.printf "+ Removing jumps to jump";
          print_newline ());
      Cfg.remove_jump_jump ();
      
      
      if !debug then 
        (Printf.printf "+ Removing jump after else";
          print_newline ());
      Cfg.permute_conds ();
          *)
          
          if !inline_only then raise Exit;
          
          if !debug then 
            (Printf.printf "+ Generating medium control flow graph";
              print_newline ());
          Print_cfg.print_globals (base^"+");
          
          Info.compute_se sorted;
          
          if !debug then 
            (Printf.printf "+ Analysing";
              print_newline ());      
          Analysis.f sorted;
          
          List.iter (fun global ->
              let func = Hashtbl.find program.desc global in
              stat_final_size := !stat_final_size + func.fun_size) 
          program.globals;
          
          if !debug then 
            (Printf.printf "+ Merging blocks";
              print_newline ());
          Cfg.make_merge_blocks ();
          
          if not !nosharing then (  
              if !debug then 
                (Printf.printf "+ Sharing blocks";
                  print_newline ());
              Cfg.share_blocks ());
          
          
          if !debug then 
            (Printf.printf "+ Generating final control flow graph";
              print_newline ());
          Print_cfg.print_globals (base ^ "++");
        end
      else
      if !debug then begin
          (Printf.printf "+ Creating start node";
            print_newline ());
          Cfg.start ();
        end
    with Exit -> ()
  end;

  
    
  List.iter (fun name -> 
      let func = Hashtbl.find program.desc name in
      func.fun_debug <- false) !debug_symbols;

  if !debug then 
    (Printf.printf "+ Saving files";
      print_newline ());
  let sopt = Printf.sprintf "%s.%s" base !extension in
  let oc = open_out sopt in
  Emit.emit oc program;
  close_out oc;
  
  clean_program ();
  let asx = program in
  let oc = open_out_bin (base ^ ".asx") in
  output_value oc asx;
  close_out oc;
  if !compile then    
    let cmd = Printf.sprintf "as -o %s.o %s" base sopt in
    if !debug then 
      (Printf.printf "+ Compiling file";
        print_newline ());
    if !verbose then (Printf.printf "Command: %s" cmd; print_newline ());
    let code = Sys.command cmd in
    (if code<>0 then failwith "Error during compilation")
    
let stat_clear () =
  stat_asx_loaded := 0;
  Emit.stat_jump_next := 0;
  Po.stat_delete_switch := 0;
  Po.stat_delete_raise := 0;
  Inline.stat_delete_frame := 0;
  Inline.stat_inline := 0;
  Inline.stat_noalloc := 0;
  Cfg.stat_block_merged := 0;
  Cfg.stat_open_functions := 0;
  Cfg.stat_functions := 0;
  Cfg.stat_shared_blocks := 0;
  Cfg.stat_shared_instrs := 0;
  Cfg.stat_permute_cond := 0;
(*  Cfg.stat_jump_jump := 0; *)
  Approx.stat_constant_prop := 0;
  Approx.stat_better_reg := 0;
  Peephole.stat_peepholes := 0;
  Optimize2.stat_dead_instrs := 0;
  Optimize2.stat_spill_deleted := 0;
  Optimize2.stat_alloc_removed := 0;
  Optimize3.stat_speculative_reloads := 0;
  Optimize4.stat_jump2jump := 0;
  stat_initial_size := 0;
  stat_final_size := 0;
  stat_max_size := 0;
  ()
  
let stat_print () =
  if !print_stats then
    begin
      Printf.printf "Statistiques:\n";
      Printf.printf "Noalloc funcs:      %d\n" !Inline.stat_noalloc;
      Printf.printf "Asx files loaded:   %d\n" !stat_asx_loaded;
      Printf.printf "Blocks merged:      %d\n" !Cfg.stat_block_merged;
      Printf.printf "Open functions:     %d\n" !Cfg.stat_open_functions;
      Printf.printf "Functions:          %d\n" !Cfg.stat_functions;
      Printf.printf "Jump to jump:       %d\n" !Optimize4.stat_jump2jump;
      Printf.printf "*** Switches deleted:   %d\n" !Po.stat_delete_switch;
      Printf.printf "*** Raise deleted:      %d\n" !Po.stat_delete_raise;
      Printf.printf "*** Frames deleted:     %d\n" !Inline.stat_delete_frame;
      Printf.printf "*** Inlining sites:     %d\n" !Inline.stat_inline;
      Printf.printf "*** Shared blocks:      %d\n" !Cfg.stat_shared_blocks;
      Printf.printf "*** Shared instrs:      %d\n" !Cfg.stat_shared_instrs;
      Printf.printf "*** Permute conds:      %d\n" !Cfg.stat_permute_cond;
(*      Printf.printf "*** Jump to jump:       %d\n" !Cfg.stat_jump_jump; *)
      Printf.printf "*** Constant integrated:%d\n" !Approx.stat_constant_prop;
      Printf.printf "*** Better register:    %d\n" !Approx.stat_better_reg;
      Printf.printf "*** Peepholes patterns: %d\n" !Peephole.stat_peepholes;
      Printf.printf "*** Dead instrs:        %d\n" !Optimize2.stat_dead_instrs;
      Printf.printf "*** Alloc removed:      %d\n" !Optimize2.stat_alloc_removed;
      Printf.printf "*** Header moved:      %d\n" !Optimize4.stat_move_header;
      Printf.printf "*** Speculative reloads:%d\n" !Optimize3.stat_speculative_reloads;
      Printf.printf "*** Combined ins:      %d\n" !Comb_types.stat_combine_ins;
      Printf.printf "*** Node unrolled:     %d\n" !Optimize4.stat_unroll;
      Printf.printf "***** %d ins ---> %d ins --> %d [%d] ****\n" !stat_initial_size !stat_max_size !stat_final_size !Emit.instr_counter;
      print_newline ();
    end
    
let optimize file =
  if
    (Filename.check_suffix file ".s") ||
    (Filename.check_suffix file ".asm") ||
    (Filename.check_suffix file ".S") 
  then optimize file
  else
    (Printf.printf "Don't know what to do with %s" file;
      print_newline ())

let _ =
  stat_clear ();
  match !files with
    [ file ] -> optimize file
  | _ ->   
      List.iter (fun file ->
          Printf.printf "%s," file; flush stderr;
          optimize file;
          Gc.compact ();
      ) (List.rev !files);
      print_newline ()

let _ =   
  stat_print ()
