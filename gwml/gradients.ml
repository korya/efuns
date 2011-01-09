(***********************************************************************)
(*                                                                     *)
(*                             GwML                                    *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

open Options
open Gwml
open Xtypes

let gradient_levels = define_option ["gradient_levels"] 
  "<gradient_levels> is the number of different colors used when computing
  a gradient." int_option 40
    
let hexs = [| '0';'1';'2';'3';'4';'5';'6';'7';'8';'9';
    'A';'B';'C';'D';'E';'F' |]
let hex1 v = hexs.((v lsr 12) mod 16)
let hex0 v = hexs.((v lsr 8) mod 16)
  
let gen_file kind r1 g1 b1 r2 g2 b2 =
  let s = 
    Printf.sprintf "/tmp/%cgradient_%c%c%c%c%c%c_%c%c%c%c%c%c.ppm" 
    kind (hex1 r1) (hex0 r1) (hex1 g1) (hex0 g1) (hex1 b1) (hex0 b1)
    (hex1 r2) (hex0 r2) (hex1 g2) (hex0 g2) (hex1 b2) (hex0 b2)
  in
  s

let rgb c = 
  match parse_color c with
    RgbColor (r,g,b) -> (r,g,b)
  | NamedColor c ->
      let colormap = Xlib.defaultColormap display in
      let lc = X.lookupColor display colormap c in
      lc.lc_exact.red, lc.lc_exact.green, lc.lc_exact.blue
      
let make_vgradient color1 color2 =
  let (r1,g1,b1) = rgb color1 in
  let (r2,g2,b2) = rgb color2 in
  let filename = gen_file 'v' r1 g1 b1 r2 g2 b2 in
  let dx = 500 in
  let dy = 19 in
  let oc = open_out filename in
  Printf.fprintf oc "P6\n%d %d\n255\n" dx (dy+1);
  for i = 0 to dy do
    let r = (r1 + (((r2 - r1) * i / dy))) lsr 8 in
    let g = (g1 + (((g2 - g1) * i / dy))) lsr 8 in
    let b = (b1 + (((b2 - b1) * i / dy))) lsr 8 in
    for j = 1 to dx do
      output_byte oc r;
      output_byte oc g;
      output_byte oc b;
    done;
  done;
  close_out oc;
  filename
  
let make_hgradient color1 color2 =
  let (r1,g1,b1) = rgb color1 in
  let (r2,g2,b2) = rgb color2 in
  let filename = gen_file 'h' r1 g1 b1 r2 g2 b2 in
  let dx = 19 in
  let n = 25 in
  let dy = 20 in
  let nlevels = !!gradient_levels in
  let nsteps = !!gradient_levels + 1 in
  let npixels_per_step = 500 / nsteps in
  let npixels = nsteps * npixels_per_step in
  let oc = open_out filename in
  Printf.fprintf oc "P6\n%d 20\n255\n" npixels; 
  for y = 0 to 19 do
    for i = 0 to nlevels do
      let r = (r1 + (((r2 - r1) * i / nlevels))) lsr 8 in
      let g = (g1 + (((g2 - g1) * i / nlevels))) lsr 8 in
      let b = (b1 + (((b2 - b1) * i / nlevels))) lsr 8 in
      for j = 1 to npixels_per_step do
        output_byte oc r;
        output_byte oc g;
        output_byte oc b;
      done;
    done;
  done;
  close_out oc;
  filename

  
let make_dgradient color1 color2 =
  let (r1,g1,b1) = rgb color1 in
  let (r2,g2,b2) = rgb color2 in
  let filename = gen_file 'd' r1 g1 b1 r2 g2 b2 in
  let dx = 19 in
  let n = 25 in
  let dy = 20 in
  let oc = open_out filename in
  Printf.fprintf oc "P6\n500 20\n255\n"; 
  for y = 0 to 19 do
    for i = 10 - y/2 to 19 - y/2 do
      let r = (r1 + (((r2 - r1) * i / 19))) lsr 8 in
      let g = (g1 + (((g2 - g1) * i / 19))) lsr 8 in
      let b = (b1 + (((b2 - b1) * i / 19))) lsr 8 in
      for j = 1 to 50 do
        output_byte oc r;
        output_byte oc g;
        output_byte oc b;
      done;
    done;
  done;
  close_out oc;
  filename
  
let check_filename filename =
  if not (Sys.file_exists filename) then
    begin (* maybe it is a generated file ... *)
      if String.length filename = 32 then
        let s = String.sub filename 0 15 in
        let c1 = String.sub filename 15 6 in 
        let c2 = String.sub filename 22 6 in
        let create = match s with
          | "/tmp/vgradient_" -> make_vgradient
          | "/tmp/hgradient_" -> make_hgradient
          | "/tmp/dgradient_" -> make_dgradient
          | _ -> raise Not_found
        in
        ignore (create ("#"^c1) ("#"^c2));
      else raise Not_found
    end;
  