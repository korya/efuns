(***********************************************************************)
(*                                                                     *)
(*                             ____________                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(*[load modname] load the bytecode file corresponding to the given module.
If the bytecode depends on other modules or interfaces, these dependencies
will be loaded before. *)
val load : string -> unit

(*[eval command] evaluate the command, and return a string corresponding to
the result. Raise an exception for compilation errors or exception results.
You can use Utils.printexn to get a string corresponding to the exception.
 (Dyneval.init will install a printer for compilation exceptions in Utils).
*)
val eval : string -> string

(*[init auth] Initialize the library. Must be called before anything.
auth is true if unsafe modules are allowed, false otherwise 
(really implemented ?) *)
val init : bool -> unit

(* [load_path] the path used to find bytecode files and interfaces. *)
val load_path : string list ref

(* [libraries] correspondance between libraries and modules *)
val libraries : (string * string list) list Options.option_record
  
(* [load_module modname] the same as [load], but doesn't print anything
in case of error. *)
val load_module : string -> unit

(* [load_file filename] the same as [load_module], but a filename is specified
  instead of a module name. *)
val load_file : string -> unit

(* [compile filename] compile a source file to a bytecode file.
Useful to avoid "ocamlc" when versions may have changed.
  *)
val compile : string -> unit