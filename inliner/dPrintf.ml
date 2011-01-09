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

(* $Id: dPrintf.ml,v 1.1 1999/10/25 07:39:40 lefessan Exp $ *)

external format_int: string -> int -> string = "format_int"
external format_float: string -> float -> string = "format_float"

let fprintf outchan format =
  let format = (Obj.magic format : string) in
  let rec doprn i =
    if i >= String.length format then
      Obj.magic ()
    else begin
        let c = String.unsafe_get format i in
        if c <> '%' then begin
            doprn (succ i)
          end else begin
            let j = skip_args (succ i) in
            match String.unsafe_get format j with
              '%' ->
                doprn (succ j)
            | 's' ->
                Obj.magic(fun (s: string) ->
                    doprn (succ j))
            | 'c' ->
                Obj.magic(fun (c: char) ->
                    doprn (succ j))
            | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
                Obj.magic(fun (n: int) ->
                    doprn (succ j))
            | 'f' | 'e' | 'E' | 'g' | 'G' ->
                Obj.magic(fun (f: float) ->
                    doprn (succ j))
            | 'b' ->
                Obj.magic(fun (b: bool) ->
                    doprn (succ j))
            | 'a' ->
                Obj.magic(fun (printer: out_channel -> 'a -> unit) (arg: 'a) ->
                    doprn(succ j))
            | 't' ->
                Obj.magic(fun (printer: out_channel -> unit) ->
                    doprn(succ j))
            | c ->
                invalid_arg ("fprintf: unknown format")
          end
      end
      
  and skip_args j =
    match String.unsafe_get format j with
      '0' .. '9' | ' ' | '.' | '-' -> skip_args (succ j)
    | c -> j

  in doprn 0

let printf fmt = fprintf stdout fmt
and eprintf fmt = fprintf stderr fmt
  