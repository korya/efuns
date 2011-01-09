(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

val lexer_passwd: Lexing.lexbuf -> Parsers.token
val lexer_filename: Lexing.lexbuf -> Parsers.token
val lexer_glob: Lexing.lexbuf -> string