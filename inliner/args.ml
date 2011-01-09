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

let files = ref ([]: string list)
let level = ref 2
let ocaml = ref false
let print_cfg = ref false
let print_ps = ref false
let path = ref ["."]
let compile = ref false
let inline_only = ref false
  
let extension = ref "s.opt"
let inlining = ref 10
let print_stats = ref false
let verbose = ref false
let noapprox = ref false
let nosharing = ref false
let noopt = ref false
let debug_live = ref false
let log = ref false
let log_file = ref ""
let debug_meet = ref false
let no_inline = ref false
let no_peephole = ref false
let dont_remove_dead = ref false
let no_data = ref false
let no_speculative_reloads = ref false
let no_fast_alloc = ref false
let no_combine = ref true
let debug_combine = ref false
let no_unroll = ref false
  
let debug_all_globals = ref true
let debug_inline = ref false
let debug = ref false
let debug_approx = ref false
let debug_opt1 = ref false
let debug_opt2 = ref false
let debug_opt3 = ref false
let debug_opt4 = ref false
let debug_peephole = ref false
let debug_symbols = ref []
let debug_nodes = ref []
let debug_po = ref false
let debug_print_code = ref false
let debug_result = ref false
let max_concat_len = ref 3
let one_debug = ref false
let catch_bound_checks = ref false
  
let _ =
  Arg.parse [ 
    "-I", Arg.String (fun s -> path := s :: !path), " <dir>: add to path";
    "-level", Arg.Int (fun i -> level := i), " <int>: optimization force";
    "-ocaml", Arg.Set ocaml, " : from Objective-Caml compiler";
    "-ext", Arg.String (fun s -> extension := s),
    " <.extension> : extension of created files";
    "-cfg2ps", Arg.Unit (fun _ ->
        print_cfg := true;
        print_ps := true),
    " : print the cfg and its postscript display";
    "-cfg", Arg.Set print_cfg, 
    " : print the cfg in graphviz format"; 
    "-c", Arg.Set compile, 
    " : compile with 'as'";
    "-inline", Arg.Int (fun i -> inlining := i),
    " <ninstrs> : threshold for inlining (default is 10)";

    "-max_concat_len", Arg.Int (fun i -> max_concat_len := i), 
    " <len> : max len for jump2jump";
    "-stats", Arg.Set print_stats, " : print some statistics";
    "-verbose", Arg.Set verbose, " : more verbosity";
    "-debug", Arg.Set debug, " : global debug mode";
    "-dapprox", Arg.Set debug_approx, " : debug approx mode";
    "-dlive", Arg.Set debug_live, " : debug mode for liveness";
    "-dmeet", Arg.Set debug_meet, " : debug mode for meets on nodes";
    "-dinline", Arg.Set debug_inline, " : debug mode for inlining";
    "-dopt1", Arg.Set debug_opt1, " : debug mode for opt1";
    "-dopt2", Arg.Set debug_opt2, " : debug mode for opt2";
    "-dopt3", Arg.Set debug_opt3, " : debug mode for opt3";
    "-dopt4", Arg.Set debug_opt4, " : debug mode for opt4";
    "-dpeephole", Arg.Set debug_peephole, " : debug mode for peephole";
    "-dpo", Arg.Set debug_po, " : debug switch deletion";
    "-dcombine", Arg.Set debug_combine, " : debug combine pass";
    "-danalysis", Arg.Unit (fun _ ->
        debug_live := true;
        debug_approx := true;
        debug_opt1 := true;
        debug_opt2 := true;
        debug_combine := true;
    ), " : debug mode for all analysis passes";
    "-dresult", Arg.Set debug_result, " : print result of approx+liveness";
    "-dprint", Arg.Set debug_print_code, " : print code between opts";
    "-fake", Arg.Unit (fun _ ->
        dont_remove_dead := true;
    ), " : disable dangerous optimizations";
    "-noapprox", Arg.Set noapprox, " : disable approx mode";
    "-nosharing", Arg.Set nosharing, " : disable sharing mode";
    "-noopt", Arg.Set noopt, " : disable all optimizations";
    "-noinline", Arg.Set no_inline, " : no inlininng at all";
    "-nopo", Arg.Set no_peephole, " : disable peephole optimizations";
    "-nodata", Arg.Set no_data, " : don't emit data segment";
    "-no_speculative_reloads", Arg.Set no_speculative_reloads,
    " : disable speculative reloads";
    "-no_fast_alloc", Arg.Set no_fast_alloc, " : no fast alloc";
    "-do_combine", Arg.Unit (fun _ ->  no_combine:= false), " : do a combine pass";
    "-log", Arg.String (fun s ->
        log := true; log_file := s) , " : enable logging of optimizations";
    "-dsymbol", Arg.String (fun s -> 
        debug_all_globals := false;
        debug_symbols := s :: !debug_symbols),
    " <symbol> : debug this symbol";
    "-dnode", Arg.Int (fun i -> debug_nodes := i :: !debug_nodes),
    " <node> : debug this node";
    "-inline_only", Arg.Set inline_only, " : emit code just after inlining";
    "-no_unroll", Arg.Set no_unroll, " : prevent loop unrolling";
    "-catch_bound_checks", Arg.Set catch_bound_checks, " : don't remove trywith for boundchecks";
  ] (fun file -> files := file :: !files) 
  "An assembler optimizer, specialized for Objective-Caml"
  
let path = List.rev !path

let log = if !log then 
    let oc = open_out !log_file in
    (fun f -> Printf.fprintf oc "%s\n" (f ()); flush oc)
  else
    (fun f -> ())

