(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: dyneval.ml,v 1.14 2001/01/19 15:59:41 lefessan Exp $ *)

(* The interactive toplevel loop *)

open Utils
open Lexing
open Format
open Config
open Misc
open Parsetree
open Types
open Typedtree
open Longident
open Compat_comp
  
let link_lambda code codesize reloc =
  let initial_symtable = Symtable.current_state() in
  Symtable.patch_object code reloc;
  Dynsyms.install_modules reloc;
  Symtable.update_global_table();
  begin try
      (Meta.reify_bytecode code codesize) ()
  with exn ->
    Symtable.restore_state initial_symtable;
    raise exn
  end
  
type directive_fun =
  Directive_none of (unit -> unit)
| Directive_string of (string -> unit)
| Directive_int of (int -> unit)
| Directive_ident of (Longident.t -> unit)

(* Hooks for parsing functions *)

let parse_toplevel_phrase = ref Parse.toplevel_phrase
let parse_use_file = ref Parse.use_file
let print_location = Location.print
let print_warning = Location.print_warning
let input_name = Location.input_name

(* Load in-core and execute a lambda term *)

type evaluation_outcome = Result of Obj.t | Exception of exn

let load_lambda lam =
  let format = Format.std_formatter in
  if !Clflags.dump_rawlambda then begin
      printlambda_lambda format lam; print_newline()
    end;
  let slam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then begin
      printlambda_lambda format slam; print_newline()
    end;
  let (init_code, fun_code) = Bytegen.compile_phrase slam in
  if !Clflags.dump_instr then begin
      printinstr_instrlist format init_code;
      printinstr_instrlist format fun_code;
      print_newline()
    end;
  let (code, code_size, reloc) = Emitcode.to_memory init_code fun_code in
  let code = Dynsyms.alloc_code code code_size in
  let can_free = (fun_code = []) in
  try
    let v = link_lambda code code_size reloc in
    if can_free then Dynsyms.free_code code;
    Result v
  with x ->
      if can_free then Dynsyms.free_code code;
      Exception x  
  
(* Print the outcome of an evaluation *)

let rec print_items vals pos env = function
    Tsig_value(id, decl)::rem ->
      let format = Format.std_formatter in
      open_box 2;
      let pos =
        match decl.val_kind with
          Val_prim p ->
            print_string "external "; printtyp_ident format id;
            print_string " :"; print_space();
            printtyp_type_scheme format decl.val_type; print_space();
            print_string "= "; primitive_print_description format p;
            pos
        | _ ->
            print_string "val "; printtyp_ident format id;
            print_string " :"; print_space();
            printtyp_type_scheme format decl.val_type;
            print_string " ="; print_space();
            print_value env vals.(pos) format decl.val_type;
            pos + 1
      in
      close_box();
      print_space (); print_items vals pos env rem
  | Tsig_type(id, decl)::rem ->
      let format = Format.std_formatter in
      printtyp_type_declaration id format decl;
      print_space (); print_items vals pos env rem
  | Tsig_exception(id, decl)::rem ->
      let format = Format.std_formatter in
      printtyp_exception_declaration id format decl;
      print_space (); print_items vals (pos+1) env  rem
  | Tsig_module(id, mty)::rem ->
      let format = Format.std_formatter in
      open_box 2; print_string "module "; printtyp_ident format id;
      print_string " :"; print_space(); printtyp_modtype format mty; close_box();
      print_space (); print_items vals (pos+1) env rem;
  | Tsig_modtype(id, decl)::rem ->
      let format = Format.std_formatter in
      printtyp_modtype_declaration id format decl;
      print_space (); print_items vals pos env rem
  | Tsig_class(id, decl)::cltydecl::tydecl1::tydecl2::rem ->
      let format = Format.std_formatter in
      printtyp_class_declaration id format decl;
      print_space (); print_items vals (pos+1) env rem
  | Tsig_cltype(id, decl)::tydecl1::tydecl2::rem ->
      let format = Format.std_formatter in
      printtyp_cltype_declaration id format decl;
      print_space (); print_items vals pos env rem
  | _ ->
      ()

(* Print an exception produced by an evaluation *)

let print_exception_outcome = function
    Sys.Break ->
      print_string "Interrupted."; print_newline()
  | Out_of_memory ->
      Gc.full_major();
      print_string "Out of memory during evaluation.";
      print_newline()
  | Stack_overflow ->
      print_string "Stack overflow during evaluation (looping recursion?).";
      print_newline();
  | exn ->
      open_box 0;
      print_string "Uncaught exception: ";
      print_string (Utils.printexn exn);
      print_newline()

(* The table of toplevel directives. 
   Filled by functions from module topdirs. *)

let directive_table = (Hashtbl.create 13 : (string, directive_fun) Hashtbl.t)

(* Execute a toplevel phrase *)

let toplevel_env = ref Env.empty
let toplevel_num = ref 1
  
let execute_phrase print_outcome phr =
  match phr with
    Ptop_def sstr ->
      let modname = Printf.sprintf "Toplevel%d" !toplevel_num in
      let modid = Ident.create_persistent modname in
      let sstr = match sstr with
          [{ pstr_desc = Pstr_eval exp; pstr_loc = l}] -> 
            let pat = { ppat_desc = Ppat_var "_";
                ppat_loc =l }
            in
            [{ pstr_desc = Pstr_value (Asttypes.Nonrecursive,[pat,exp]);
              pstr_loc = l }]

        | _ -> sstr
      in
      let (str, sg, newenv) = Typemod.type_structure !toplevel_env sstr in
      let lam = Translmod.transl_implementation modname (str, Tcoerce_none) in
      let res = load_lambda lam in
      begin match res with
          Result v ->
            if print_outcome then begin
                open_vbox 0;
                let modvals = Obj.magic (Symtable.get_global_value modid) in
                print_items modvals 0 newenv sg;
                close_box();
                print_flush();
              end;
            let sg = List.fold_right (fun item sg ->
                  match item with
                    | Tsig_type _ 
                    | Tsig_modtype _
                  | Tsig_cltype _ -> sg
                  | _ -> item :: sg
                      ) sg [] in
            toplevel_env :=  Env.open_signature (Path.Pident modid) sg newenv;
            (*            toplevel_env := newenv; *)
            incr toplevel_num;
            true
        | Exception exn ->
            print_exception_outcome exn;
            false
      end
  | Ptop_dir(dir_name, dir_arg) ->
      try
        match (Hashtbl.find directive_table dir_name, dir_arg) with
          (Directive_none f, Pdir_none) -> f (); true
        | (Directive_string f, Pdir_string s) -> f s; true
        | (Directive_int f, Pdir_int n) -> f n; true
        | (Directive_ident f, Pdir_ident lid) -> f lid; true
        | (_, _) ->
            print_string "Wrong type of argument for directive `";
            print_string dir_name; print_string "'"; print_newline();
            false
      with Not_found ->
          print_string "Unknown directive `"; print_string dir_name;
          print_string "'"; print_newline();
          false
          
(* Temporary assignment to a reference *)
let protect r newval body =
  let oldval = !r in
  try
    r := newval; 
    let res = body() in
    r := oldval;
    res
  with x ->
      r := oldval;
      raise x

(* Read and execute commands from a file *)

let use_print_results = ref true

let use_file name =
  try
    let filename = find_in_path !Config.load_path name in
    let ic = open_in_bin filename in
    let lb = Lexing.from_channel ic in
    (* Skip initial #! line if any *)
    let buffer = String.create 2 in
    if input ic buffer 0 2 = 2 && buffer = "#!"
    then ignore(input_line ic)
    else seek_in ic 0;
    let success =
      protect Location.input_name filename (fun () ->
          try
            List.iter
              (fun ph ->
                if execute_phrase !use_print_results ph then () else raise Exit)
            (!parse_use_file lb);
            true
          with
            Exit -> false
          | Sys.Break ->
              print_string "Interrupted."; print_newline(); false
          | x ->
              let format = Format.std_formatter in
              errors_report_error format x; false) in
    close_in ic;
    success
  with Not_found ->
      print_string "Cannot find file "; print_string name; print_newline();
      false

let use_silently name =
  protect use_print_results false (fun () -> use_file name)

(* Reading function for interactive use *)

let first_line = ref true
let got_eof = ref false;;

let refill_lexbuf buffer len =
  if !got_eof then (got_eof := false; 0) else begin
      output_string stdout (if !first_line then "# " else "  "); flush stdout;
      first_line := false;
      let i = ref 0 in
      try
        while !i < len && (let c = input_char stdin in buffer.[!i] <- c; c<>'\n')
        do incr i done;
        !i + 1
      with End_of_file ->
          Location.echo_eof ();
          if !i > 0
          then (got_eof := true; !i)
          else 0
    end

(* Discard everything already in a lexer buffer *)

let empty_lexbuf lb =
  let l = String.length lb.lex_buffer in
  lb.lex_abs_pos <- (-l);
  lb.lex_curr_pos <- l

(* Toplevel initialization. Performed here instead of at the
   beginning of loop() so that user code linked in with ocamlmktop
   can call directives from Topdirs. *)

let _ =
  Sys.interactive := true;
  Clflags.thread_safe := true;
  Compile.init_path()

let load_ocamlinit () =
  if Sys.file_exists ".ocamlinit" then ignore(use_silently ".ocamlinit")

(* The interactive loop *)

exception PPerror

let loop() =
  print_string "        Objective Caml version ";
  print_string Config.version;
  print_newline(); print_newline();
  (* Add whatever -I options have been specified on the command line,
     but keep the directories that user code linked in with ocamlmktop
     may have added to load_path. *)
  load_path := "" :: (List.rev !Clflags.include_dirs @ !load_path);
  toplevel_env := Compile.initial_env();
  let lb = Lexing.from_function refill_lexbuf in
  Location.input_name := "";
(*  Location.input_lexbuf := Some lb;  *)
  Sys.catch_break true;
  load_ocamlinit ();
  while true do
    try
      empty_lexbuf lb;
      Location.reset();
      first_line := true;
      let phr = try !parse_toplevel_phrase lb with Exit -> raise PPerror in
      ignore(execute_phrase true phr)
    with
      End_of_file -> exit 0
    | Sys.Break ->
        print_string "Interrupted."; print_newline()
    | PPerror -> ()
    | x ->
        let format = Format.std_formatter in
        errors_report_error format x
  done

(* Execute a script *)

let run_script name =
  Compile.init_path();
  toplevel_env := Compile.initial_env();
  Format.set_formatter_out_channel stderr;
  use_silently name
  
let init auth =
  toplevel_env := Compile.initial_env();
  let format = Format.std_formatter in
  Utils.register_exn (Utils.format_to_string (errors_report_error format));
  Dynlink.allow_unsafe_modules auth;
  Dynlink.init ()
  
  (* The string should terminate with ;; *)
let eval_string s =
  let lb = Lexing.from_string s in
  Location.input_name := "";
(*  Location.input_lexbuf := Some lb; *)
  let phr = !parse_toplevel_phrase lb in
  ignore(execute_phrase true phr)
  
let eval s = Utils.format_to_string eval_string s

open Options
  
  
let load_path = ref []
    
let libraries = define_option ["libraries_modules"] 
  "<libraries_modules> is a list containing, for each library, the modules
  which are contained in it."
    (list_option (tuple2_option (filename_option, smalllist_option string_option)))
  ["stdlib.cma", ["Parsing"]]

let debug_load = define_option ["debug_load"] "" bool_option false

let rec load_module mod_name =
  if !!debug_load then
    Log.printf "load_module %s\n" mod_name;
  let filename = (String.uncapitalize mod_name) ^ ".cmo" in
  try
    let filename = Utils.find_in_path !load_path filename in
    load_file filename
  with Not_found -> 
      if !!debug_load then
        Log.printf "load_module Not_found %s\n" mod_name;
      try
        List.iter (fun (filename, list) ->
            if List.mem mod_name list then
              let mod_name = ":" ^ mod_name in
              try
                let filename = Utils.find_in_path !load_path filename in
                load_file (filename ^ mod_name);
                raise Exit
              with
                Not_found -> failwith ("Can't find file "^filename)
              | Sys_error _ ->
          (* Should only append using the bytecode dynlink library
                  which does not understand the syntax lib:mod *)
                  load_file filename;
                  raise Exit
        ) !!libraries;
        failwith ("Can't find file "^filename);
      with Exit -> 
          Log.printf "load_module Exit %s\n" mod_name
          
and load_file filename =
  if !!debug_load then
    Log.printf "load_file %s\n" filename;
  try
    Dynlink.loadfile filename
  with
    Dynlink.Error (Dynlink.Unavailable_unit mod_name) -> 
      load_interface mod_name;
      load_file filename
  | Dynlink.Error (
    Dynlink.Linking_error (_, Dynlink.Undefined_global mod_name)) ->
      load_module mod_name;
      load_file filename

and load_interface mod_name =
  if !!debug_load then
    Log.printf "load_interface: %s\n" mod_name;
  try
    Dynlink.add_interfaces [mod_name] !load_path; ()
  with
    Not_found ->
      failwith (Printf.sprintf "No interface for %s" mod_name)

let load mod_name =
  if !!debug_load then
    Log.printf "load %s\n" mod_name;
  try
    load_module mod_name
  with
    Dynlink.Error e -> 
      Printf.printf "Dynlink error: %s" (Dynlink.error_message e);
      print_newline () 
  | e ->
      Printf.printf "Dynlink runtime error: %s" (Printexc.to_string e);
      print_newline () 

let compile f = 
  let format = Format.std_formatter in
  compile_implementation format f
  