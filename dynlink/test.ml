open Instruct
open Printf
open Interp
  
let _ =
  Dynlink.init (); (* initialize dynlink *)
  Dynlink.allow_unsafe_modules true;
  Dynlink.add_available_units [] (* make some modules available []=all *)
(*
  ;at_exit (fun () -> 
      for i=0 to opBREAK do
        printf "%s: %d\n" opnames.(i) instructs.(i)
      done;
      print_newline ())
  *)
  
  
let inc = open_in "cmos" 
let _ =
  try
    while true do
      let filename = (input_line inc) in
      Dynlink.loadfile filename;
    done;
  with
    Dynlink.Error error -> 
      print_string (Dynlink.error_message error); print_newline ()
  | End_of_file -> ()

let _ = 
  close_in inc;
  
