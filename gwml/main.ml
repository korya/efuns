(***********************************************************************)
(*                                                                     *)
(*                           Gwml                                      *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Options
open Gwml_args
open Xtypes
open Gwml  
  
(* Register exception handlers *)
let _ =
  Utils.register_exn (fun exn ->
      try raise exn with
        XError e -> 
          Xlib.xerror_to_string e
      | GrabError s ->
          match s with
          | AlreadyGrabbed ->  "GrabError: AlreadyGrabbed"
          | InvalidTime -> "GrabError: InvalidTime"
          | NotViewable -> "GrabError: NotViewable"
          | Frozen -> "GrabError: Frozen"
          | _ -> "GrabError: Success"
          );
  Utils.register_exn (fun exn ->
      try raise exn with
        Unix.Unix_error (e,s1,s2) ->
          Printf.sprintf "Unix error: %s: %s (%s)" s1 (Unix.error_message e) s2)

let ddisp = Eloop.add_display  display 
    (fun ev -> 
      if !!debug_events then
        (Log.printf "Default Handler receive : %s\n" 
            (event_to_string ev));
  )

let cdisp = Eloop.add_display display
    (fun ev ->
      if !!debug_events then
        (Log.printf "Default Handler receive : %s\n" 
          (event_to_string ev));
  )

let compact_at_startup = 
  define_option ["compact_at_startup"] 
  "<compact_at_startup> is <true> if you want GwML to compact the heap
  just after loading all configuration files. This is normally a good thing,
  unless you are using libraries which don't support heap compaction (threads)." bool_option true

let gwmldir = Filename.concat Utils.homedir 
    (".gwml-" ^ Version.gwml_version)

let make_at_startup = define_option ["make_at_startup"] 
    (Printf.sprintf "<make_at_startup> should be <true> if you want gwml to try to
    compile your own Ocaml files using <make> at startup in
    your %s directory." gwmldir) bool_option true
  
let error_in_make = define_option ["error_in_make"] 
  "<error_in_make> set the behavior of GwML when an error occurs while
  compiling your Ocaml files. Possible choices are <abort>, <continue> or
  <revert> to remove your directory from the path and load the standard
  configuration." string_option "revert"
  
let _ =
  if !!make_at_startup then
    if Sys.file_exists gwmldir then
      match  Sys.command (Printf.sprintf 
          "cd %s; make depend; make" gwmldir) with
        0 -> Printf.printf "Compilation was OK"; print_newline ();
      | n -> 
          Printf.printf "Exit status: %d (ERROR)" n;
          print_newline ();
          match !!error_in_make with
            "abort" -> exit 2
          | "continue" -> ()
          | "revert" ->
              Dyneval.load_path := Utils.list_remove !Dyneval.load_path
              gwmldir
          | _ -> ()
let main () =      
 
  (* Update signals *)
  Unix.set_close_on_exec display.socket;
  Utils.set_signal Sys.sigint  (Sys.Signal_handle Wob.exit_gwml);
  Utils.set_signal Sys.sigterm (Sys.Signal_handle Wob.exit_gwml);
  Utils.set_signal Sys.sighup  (Sys.Signal_handle Wob.restart);

  (* Load configuration files *)  
  
  Dyneval.init true;
  if not !config_loaded then begin
      Printf.printf "Loading configuration file gwmlrc.cmo"; print_newline ();
      if not !no_gwmlrc then Dyneval.load "Gwmlrc"; 
    end;
  Printf.printf "Loaded"; print_newline ();
  let rec iter list =
    match list with
      [] -> ()
    | f :: tail ->
        Dyneval.load f;
        iter tail
  in
  iter !load_files;
  
  (* late, execute .gwmlrc.ml *)
  if (not !batch_mode) || !gwml_talk then
    Utils.catchexn ".gwmlrc.ml:" (fun _ ->
        Printf.printf "Loading configuration file .gwmlrc.ml"; 
        print_newline ();
        
        let file = Utils.find_in_path (Utils.homedir :: !!load_path)
          ".gwmlrc.ml" in
        let ic = open_in file in
        let s = ref "" in
        
        let b = String.create 1000 in
        let rec iter () =
          let len = input ic b 0 1000 in
          if len > 0 then
            (s := !s ^ (String.sub b 0 len); iter ())
        in
        iter ();
        let s = Dyneval.eval (!s ^ " ;;") in
        let len = min 300 (String.length s) in
        Printf.printf ".gwmlrc.ml:%s" (String.sub s 0 len);
        print_newline ()
    );

  if !help then Options.help stdout;
  if !install then begin
      Printf.printf "Saving .gwmlrc after install"; print_newline ();
      Options.save ();
    end;
  Printf.printf "Decorating screens..."; print_newline ();
  (* initialize the screens *)
  Gwml.grabServer ();
  Utils.catchexn "Decorate screen:" (fun _ ->
      screens := Array.mapi (fun i root ->
          Screen.make root ddisp i) display.dpy_roots);
  Gwml.ungrabServer ();
  Printf.printf "...Screen decorated"; print_newline ();
  
  exec_hooks () !final_actions;
  prevent_animation := false;
  
  if !gwml_talk then
    begin
      Printf.printf "Gwml talk:"; print_newline ();
      let tampon = String.create 1000 in
      let str = ref "" in
      Concur.Thread.add_reader (Unix.stdin) 
      (function () ->
            let len = input stdin tampon 0 1000 in
            if len = 0 then
              Concur.Thread.remove_reader (Unix.stdin)
            else
            let l = String.length !str in
            let s = String.create (l + len) in
            String.blit !str 0 s 0 l;
            String.blit tampon 0 s l len;
            let rec iter i =
              if i > 0 then
                if s.[i] = ';' && s.[i-1] = ';' then
                  let rep = 
                    try
                      let sub = String.sub s 0 (i+1) in
                      let rep = Dyneval.eval sub in
                      rep
                    with
                      e -> 
                        Utils.printexn e
                  in
                  str := String.sub s (i+1) (len+l-i-1);
                  print_string rep;
                  print_newline ()
                else
                  iter (i-1)
              else
                str := s
            in
            iter (len+l-1)
      )
    end;
    (* If no .gwmlrc, create one ... *)
  let gwmlrc = Filename.concat Utils.homedir ".gwmlrc" in
  if not (Sys.file_exists gwmlrc) then begin
      Printf.printf "Creating .gwmlrc in HOME directory";
      print_newline ();
      Options.save ();
    end;
  Printf.printf "Entering Event loop"; print_newline ();
   (* Enter the event loop *)
  if !gwml_talk || not !batch_mode || !loop then
    begin
      if !!compact_at_startup then Gc.compact ();
      Eloop.event_loop ()
    end
  
(* Remove all decorations *)  
let _ = 
  Utils.catchexn "Main.main()" main;
  Wob.exit_gwml ()
  
