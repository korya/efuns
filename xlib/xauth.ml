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


open Xtypes

let auth_filename = ".Xauthority"
let home = Sys.getenv "HOME"

let filename = home ^ "/" ^ auth_filename

let read_short file = 
  let c0 = input_byte file in
  let c1 = input_byte file in
    c0*256+c1

let read_counted_string file =
  let len = read_short file in
  let str = String.create len
  in
    for i=0 to len-1 do
      str.[i] <- Char.chr (input_byte file)
    done;
    str

exception UnknownFamily
let readOneAuth file =
  let family = 
    match read_short file with
        256 -> FamilyLocal
      | 0xFFFF -> FamilyWild
      | 254 -> FamilyNetname
      | 253 -> FamilyKrb5Principal
      | 252 -> FamilyLocalHost
      | 0 -> FamilyIP
      | _ -> raise UnknownFamily
  in
    let address = read_counted_string file in
    let number = read_counted_string file in
    let name = read_counted_string file in
    let data = read_counted_string file in
    {
      xauth_family = family;
      xauth_address = address;
      xauth_number = number;
      xauth_name = name;
      xauth_data = data
    }

let print_hexa v =
  print_char (Char.chr 
                (if v>9 then
                  Char.code 'a' + v - 10
                else
                  Char.code '0' + v))

let print_hex v =
  print_hexa (v / 16);
  print_hexa (v mod 16)

let print xauth =
  let (f,address) =
    match xauth.xauth_family with
        FamilyLocal -> "Local", xauth.xauth_address
      | FamilyWild -> "Wild", xauth.xauth_address
       | FamilyNetname -> "Netname", xauth.xauth_address
       | FamilyKrb5Principal -> "Krb5", xauth.xauth_address
       | FamilyLocalHost -> "Localhost", xauth.xauth_address
       | FamilyIP -> "IP", Unix.string_of_inet_addr 
                              (Obj.magic xauth.xauth_address)

  in
    Printf.printf "%-10s %-20s:%-3s %-20s " 
      f address xauth.xauth_number xauth.xauth_name;
    for i = 0 to String.length xauth.xauth_data - 1 do
      print_hex (Char.code xauth.xauth_data.[i])
    done;
    print_newline ()

let best_names = [| "MIT-MAGIC-COOKIE-1" |]

let getBestAuth auth =
  let file = try open_in_bin filename with _ -> raise Not_found in
  let best = ref None in
  let best_name = ref (Array.length best_names) in
  begin
    try
      while true do
        let entry = readOneAuth file in
        if 
            ((auth.xauth_family = FamilyWild) ||
              (entry.xauth_family = FamilyWild) ||
              ((auth.xauth_family = entry.xauth_family) &&
                (auth.xauth_address = entry.xauth_address))) &&
            ((auth.xauth_number =  entry.xauth_number) ||
              (auth.xauth_number ="") ||
              (entry.xauth_number ="")) then
            begin
              if !best_name = 0 then 
                best := Some entry
              else
              try
                for i=0 to !best_name do
                  if best_names.(i) = entry.xauth_name then
                    begin
                      best := Some entry;
                      best_name := i;
                      raise  Exit
                    end
                done
              with Exit -> 
                if !best_name = 0 then
                  raise End_of_file
            end
        done;
      with
        _ -> close_in file
    end;
  match !best with
      Some auth -> 
        (auth.xauth_name,auth.xauth_data)
    | None -> raise Not_found

      