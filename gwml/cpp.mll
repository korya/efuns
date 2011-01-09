{
(***********************************************************************)
(*                                                                     *)
(*                             ____                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
  
  let current_file = ref []
  let current_line = ref ""
  let current_pos = ref 0
  
  let abort () =
    let location = match !current_file with
        [] -> assert false
      | (_,filename,line,_)::_ -> Printf.sprintf " at %s:%d" filename !line
    in
    List.iter (fun (ic,_, _, _) -> close_in ic) !current_file;
    current_file := []; 
    current_line := "";
    location
  
  let failwith s = 
    let location = abort () in
    failwith (s^location)
  
  let path = ref []
  let maxstr = 100
  let strbuf = String.create maxstr
  let strlen = ref 0
  let add_char lexbuf =
    strbuf.[!strlen] <- Lexing.lexeme_char lexbuf 0;
    incr strlen;
    if !strlen = maxstr then failwith "String too long in cpp"

}

rule next_line = parse
    "#include" [' ' '\t']* '<' { strlen:=0; include_file lexbuf }
  | "#include" [' ' '\t']* '"' { strlen:=0; include_file lexbuf }
  | [^ '\n'] * '\n' { Lexing.lexeme lexbuf }
  | eof             { 
      match !current_file with
        [] -> assert false
      | [ic,_,_,_] -> close_in ic; ""
      | (ic,_,_,_) :: (((_,_,_,lexbuf) :: _) as file) ->
          close_in ic;
          current_file := file;
          next_line lexbuf
    }

and include_file = parse
    '>' | '"' { 
      let filename = String.sub strbuf 0 !strlen in
      let filename = Utils.string_to_filename filename in
      let filename =
        try Utils.find_in_path !path filename with _ ->
            failwith (Printf.sprintf "included file %s not found" filename)
      in
      let ic = open_in filename in
      let lexbuf = Lexing.from_channel ic in
      current_file := (ic,filename,ref 0,lexbuf) :: !current_file;
      next_line lexbuf
    }
  | eof { failwith "EOF in #include directive" }
  | _ { add_char lexbuf; include_file lexbuf }

{

let do_next_line buf maxlen =
  let len = String.length !current_line in
  if len - !current_pos = 0 then begin
      current_pos := 0;
      current_line := (match !current_file with
          [] -> assert false
        | (ic,_,line, lexbuf) :: _ -> incr line; next_line lexbuf);
    end;
  let len = String.length !current_line in
  let displen = len - !current_pos in
  if displen = 0 then 0 else
  let dolen = min maxlen displen in
  String.blit !current_line !current_pos buf 0 dolen;
  current_pos := !current_pos + dolen;
  dolen

let preprocess filename =
  let ic = open_in filename in
  current_file := [ic, filename, ref 0, Lexing.from_channel ic];
  Lexing.from_function do_next_line
}