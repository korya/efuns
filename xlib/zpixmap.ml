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


(*
   Ce fichier a pour but de simplifier l'utilisation des images

Trois formats :
- Bitmap
- XYPixmap
- ZPixmap
*)

open Xtypes;;

type t = {
    image : string;
    raw_image : string;
    dx : size;
    dy : size;
    depth : int;
    bits_per_pixel : int;
    bytes_per_line : int;
    bytes_order : int array;
    bytes_per_unit : int;
    translate_bits : (char -> char);
    setPixel : (t -> coord -> coord -> pixel -> unit);
    getPixel : (t -> coord -> coord -> pixel);
    mutable modified : bool
  }

(*
  le nombre de bits par pixel est un multiple de 8
*)

module Pixel = struct
    module Unit_is_byte = struct
        module AnyBytes = struct
            let set_pixel image x y (color : pixel) =
              let color = pixel_to_id color in
              let bytes_per_pixel = image.bits_per_pixel lsr 3 in
              let pos = (y * image.bytes_per_line) + (x * bytes_per_pixel)
              and color = ref color in
              for i=0 to bytes_per_pixel -1 do
                image.image.[pos+i] <- Char.chr (!color land 255);
                color := (!color) lsr 8
              done
            
            let get_pixel image x y =
              let bytes_per_pixel = image.bits_per_pixel lsr 3 in
              let pos = (y * image.bytes_per_line) + (x * bytes_per_pixel)
              and color = ref 0 in
              for i=0 to bytes_per_pixel -1 do
                color := (!color) lor (
                  (Char.code image.image.[pos+i])
                  lsl (i lsl 3))
              done;
              (id_to_pixel !color : pixel)
          end
        
        module OneByte = struct
            
            let set_pixel image x y (color : pixel) =
              let color = pixel_to_id color in
              let pos = (y * image.bytes_per_line) + x in
              image.image.[pos] <- Char.chr color
            
            let get_pixel image x y =
              let pos = (y * image.bytes_per_line) + x in
              id_to_pixel (Char.code image.image.[pos])
                
          end
      end

(*
  le nombre de bits par pixel est 1 (bitmap)
*)
    module Unit_is_bit = struct
        let set_pixel image x y (color : pixel) =
          let color = pixel_to_id color in
          let pos = ( y * image.bytes_per_line ) + (x lsr 3)
          and offset = x land 7
          in 
          image.image.[pos] <- Char.chr (
            if color <> 0 then
              (Char.code image.image.[pos]) lor (1 lsl offset)
            else
              (Char.code image.image.[pos]) land lnot (1 lsl offset)
          )
        
        let get_pixel image x y =
          let pos = ( y * image.bytes_per_line ) + (x lsr 3)
          and offset = x land 7
          in 
          if (Char.code image.image.[pos]) land (offset lsl offset) <> 0 then
            id_to_pixel 1
          else
            id_to_pixel 0
      end

(*
  le nombre de bits par pixel n'est ni 1 ni multiple de 8
*)
    module Unit_is_bits = struct
        let set_pixel image x y (color : pixel) =
          let color = pixel_to_id color in
          let pos = ( y * image.bytes_per_line ) +
              ((x * image.bits_per_pixel)lsr 3) 
          and offset = (x * image.bits_per_pixel) land 7
          in
        (* ecrire les premiers bits *)
          let first_len = min (8-offset) image.bits_per_pixel
          in
          let first_bits = (color lsl offset) land 255
          and first_mask = ((1 lsl first_len) -1) lsl offset
          in
          image.image.[pos] <- Char.chr (
            ((Char.code image.image.[pos]) land lnot first_mask)
            lor first_bits
          );
          let left_len = ref (image.bits_per_pixel-first_len)
          and pos = ref (pos+1)
          and color = ref (color lsr first_len)
          in
          while (!left_len) >= 8 do
            left_len := (!left_len) - 8;
            image.image.[!pos] <- Char.chr ( (!color) land 255);
            color := (!color) lsr 8;
            incr pos
          done;
          if (!left_len) > 0 then
            image.image.[!pos] <- Char.chr (
              ((Char.code image.image.[!pos])
                land lnot ((1 lsl (!left_len))-1))
              lor (!color)
            )
        
        let get_pixel image x y =
          let pos = ( y * image.bytes_per_line ) +
              ((x * image.bits_per_pixel)lsr 3) 
          and offset = (x * image.bits_per_pixel) land 7
          and color = ref 0
          and bits_read = ref 0
          in
        (* ecrire les premiers bits *)
          let first_len = min (8-offset) image.bits_per_pixel
          in
          let first_bits = ((!color) lsl offset) land 255
          and first_mask = ((1 lsl first_len) -1) lsl offset
          in
          color := ((Char.code image.image.[pos]) land first_mask)
          lsr offset;
          bits_read := first_len;
          let left_len = ref (image.bits_per_pixel-first_len)
          and pos = ref (pos+1)
          in
          while (!left_len) >= 8 do
            left_len := (!left_len) - 8;
            color := ((!color) lor (
                (Char.code image.image.[!pos])
                lsl (!bits_read)
              ));
            bits_read := (!bits_read)+8;
            incr pos
          done;
          if (!left_len) > 0 then
            color := (!color) lor (
              ((Char.code image.image.[!pos]) land
                  ((1 lsl (!left_len))-1)
              )
              lsl (!bits_read)
            );
          id_to_pixel !color
      
      end
    
    let get_pixel_transform bits_per_pixel =
      if bits_per_pixel = 1 then
        (Unit_is_bit.get_pixel,Unit_is_bit.set_pixel)
      else
      if (bits_per_pixel land 7) = 0 then
        if bits_per_pixel = 8 then
          (Unit_is_byte.OneByte.get_pixel,Unit_is_byte.OneByte.set_pixel)
          else
          (Unit_is_byte.AnyBytes.get_pixel,Unit_is_byte.AnyBytes.set_pixel)
          else
        (Unit_is_bits.get_pixel,Unit_is_bits.set_pixel)
  
  end
(*
  On transforme une image formattee pour notre travail en une image
  utilisable par le display
*)
module Raw_image = struct
    let to_ximage image =
      if image.modified then
        for y = 0 to image.dy -1 do
          for x = 0 to (image.bytes_per_line/image.bytes_per_unit) -1 do
            for i = 0 to image.bytes_per_unit -1 do
              image.raw_image.[ 
                y*image.bytes_per_line + x*image.bytes_per_unit + i
              ] <-
                image.translate_bits
                image.image.[
                y*image.bytes_per_line + x*image.bytes_per_unit +
                  image.bytes_order.(i)
              ]
            done
          done
        done;
      image.modified <- false
    
    let from_ximage image =
      if image.modified then
        for y = 0 to image.dy -1 do
          for x = 0 to (image.bytes_per_line/image.bytes_per_unit) -1 do
            for i = 0 to image.bytes_per_unit -1 do
              image.image.[
                y*image.bytes_per_line + x*image.bytes_per_unit +
                  image.bytes_order.(i)
              ] <- image.translate_bits
                image.raw_image.[ 
                y*image.bytes_per_line + x*image.bytes_per_unit + i
              ]
            
            done
          done
        done;
      image.modified <- false
  end
  
(*
  En fonction du display, on trouve la fonction translate_bits
*)
module Bits_order = struct
    let inverse_bits = [|
        0x00; 0x80; 0x40; 0xc0; 0x20; 0xa0; 0x60; 0xe0;
        0x10; 0x90; 0x50; 0xd0; 0x30; 0xb0; 0x70; 0xf0;
        0x08; 0x88; 0x48; 0xc8; 0x28; 0xa8; 0x68; 0xe8;
        0x18; 0x98; 0x58; 0xd8; 0x38; 0xb8; 0x78; 0xf8;
        0x04; 0x84; 0x44; 0xc4; 0x24; 0xa4; 0x64; 0xe4;
        0x14; 0x94; 0x54; 0xd4; 0x34; 0xb4; 0x74; 0xf4;
        0x0c; 0x8c; 0x4c; 0xcc; 0x2c; 0xac; 0x6c; 0xec;
        0x1c; 0x9c; 0x5c; 0xdc; 0x3c; 0xbc; 0x7c; 0xfc;
        0x02; 0x82; 0x42; 0xc2; 0x22; 0xa2; 0x62; 0xe2;
        0x12; 0x92; 0x52; 0xd2; 0x32; 0xb2; 0x72; 0xf2;
        0x0a; 0x8a; 0x4a; 0xca; 0x2a; 0xaa; 0x6a; 0xea;
        0x1a; 0x9a; 0x5a; 0xda; 0x3a; 0xba; 0x7a; 0xfa;
        0x06; 0x86; 0x46; 0xc6; 0x26; 0xa6; 0x66; 0xe6;
        0x16; 0x96; 0x56; 0xd6; 0x36; 0xb6; 0x76; 0xf6;
        0x0e; 0x8e; 0x4e; 0xce; 0x2e; 0xae; 0x6e; 0xee;
        0x1e; 0x9e; 0x5e; 0xde; 0x3e; 0xbe; 0x7e; 0xfe;
        0x01; 0x81; 0x41; 0xc1; 0x21; 0xa1; 0x61; 0xe1;
        0x11; 0x91; 0x51; 0xd1; 0x31; 0xb1; 0x71; 0xf1;
        0x09; 0x89; 0x49; 0xc9; 0x29; 0xa9; 0x69; 0xe9;
        0x19; 0x99; 0x59; 0xd9; 0x39; 0xb9; 0x79; 0xf9;
        0x05; 0x85; 0x45; 0xc5; 0x25; 0xa5; 0x65; 0xe5;
        0x15; 0x95; 0x55; 0xd5; 0x35; 0xb5; 0x75; 0xf5;
        0x0d; 0x8d; 0x4d; 0xcd; 0x2d; 0xad; 0x6d; 0xed;
        0x1d; 0x9d; 0x5d; 0xdd; 0x3d; 0xbd; 0x7d; 0xfd;
        0x03; 0x83; 0x43; 0xc3; 0x23; 0xa3; 0x63; 0xe3;
        0x13; 0x93; 0x53; 0xd3; 0x33; 0xb3; 0x73; 0xf3;
        0x0b; 0x8b; 0x4b; 0xcb; 0x2b; 0xab; 0x6b; 0xeb;
        0x1b; 0x9b; 0x5b; 0xdb; 0x3b; 0xbb; 0x7b; 0xfb;
        0x07; 0x87; 0x47; 0xc7; 0x27; 0xa7; 0x67; 0xe7;
        0x17; 0x97; 0x57; 0xd7; 0x37; 0xb7; 0x77; 0xf7;
        0x0f; 0x8f; 0x4f; 0xcf; 0x2f; 0xaf; 0x6f; 0xef;
        0x1f; 0x9f; 0x5f; 0xdf; 0x3f; 0xbf; 0x7f; 0xff
      |]
    
    let inverse_char = Array.map (function i -> Char.chr i) inverse_bits
    
    let get_translate_bits dpy =
      match dpy.dpy_bitmap_format_bit_order with
        LeastSigniFirst -> (function i -> i)
      | MostSigniFirst -> (function i -> inverse_char.(Char.code i))
  end

(*
  En fonction du display, on trouve le tableau butes_order
*)
module Bytes_order = struct
    let tab1 =           [|
        [|0;1;2;3;0;1;2;3|];
        [|0;1;2;3;0;1;2;3|];
        [|0;1;2;3;0;1;2;3|];
        [|0;0;0;0;0;0;0;0|];
        [|0;1;2;3;0;1;2;3|]
      |]
    
    let tab2 =           [|
        [|0;1;2;3;0;1;2;3|];
        [|0;1;2;3;0;1;2;3|];
        [|1;0;3;2;1;0;3;2|];
        [|0;0;0;0;0;0;0;0|];
        [|3;2;1;0;3;2;1;0|]
      |]
      
    let get_byte_order dpy =
      (
        if dpy.dpy_image_byte_order = dpy.dpy_bitmap_format_bit_order then
          tab1 else tab2
      ).(dpy.dpy_bitmap_format_scanline_unit lsr 3)
  end

exception DepthNotFound;;
(* bits_for_depth *)
let bits_for_depth dpy depth =
  let rec iter depths =
    match depths with
      [] -> raise DepthNotFound
    |   { pxf_depth = d;
        pxf_bits_per_pixel = bits
      }::list ->
        if depth = d then
          bits
        else
          iter list
  in
  iter dpy.dpy_pixmap_formats
;;                        

(* bytes_per_line *)
let bytes_per_line bits pad width =
  (((( bits * width ) - 1 )
      / pad) + 1) *
  ( pad lsr 3)
;;

  (* newImage *)
let newImage dpy width height depth image raw_image =
  let bits_per_pixel =  bits_for_depth dpy depth
  in let bytes_per_line =  bytes_per_line bits_per_pixel 
      dpy.dpy_bitmap_format_scanline_pad width
  in 
  let (get_pixel,set_pixel) = Pixel.get_pixel_transform bits_per_pixel
  in
  {
    image = if image= "" then
      String.make (bytes_per_line * height) '\000'
    else
      image;
    raw_image = if raw_image = "" then
      String.create (bytes_per_line * height)
    else
      raw_image;
    dx = width;
    dy = height;
    depth = depth;
    bits_per_pixel = bits_per_pixel;
    bytes_per_line = bytes_per_line;
    setPixel = set_pixel;
    getPixel = get_pixel;
    bytes_order = Bytes_order.get_byte_order dpy;
    translate_bits = Bits_order.get_translate_bits dpy;
    bytes_per_unit = dpy.dpy_bitmap_format_scanline_unit lsr 3;
    modified = true
  }
  
  (* putImage *)
let putImage dpy win gc x y zpixmap =
  Raw_image.to_ximage zpixmap;
  let gg = X.getGeometry dpy win in
  let image = zpixmap.raw_image in
  let len = String.length image in
  let perline = len / zpixmap.dy in
  assert (perline * zpixmap.dy = len);
  let maxlines = (dpy.dpy_max_request_length - 50) / perline in
  let rec iter y vy dy =
    let ny = min maxlines dy in
    X.putImage dpy gc win x y zpixmap.dx ny
      0 zpixmap.depth ZPixmap 
    (String.sub image (vy * perline) (ny * perline));
    if ny < dy then
      iter (y+ny) (vy+ny) (dy-ny)
  in
  iter y 0 zpixmap.dy

  (* createZPixmap *)
let create dpy width height depth =
  newImage dpy width height depth  "" ""

  (*[getImage]  getImage dpy win x y width height planes *)
let getImage dpy win x y width height planes =
  let image =
    X.getImage dpy win x y width height planes ZPixmap
  in 
  let res = newImage dpy width height image.gi_depth "" image.gi_image
  in
  Raw_image.from_ximage res;
  res

let setPixel pix x y color =
  pix.setPixel pix x y color;
  pix.modified <- true

let getPixel pix x y =
  pix.getPixel pix x y
  
  