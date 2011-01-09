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
  
(* seule assertion sur le codage des types concrets:
  int_of_enum,enum_of_int,mask_of_list,list_of_mask l'utilise
 *)
let int_of_enum v = ((Obj.magic v) : int)
let enum_of_int (v : int) = Obj.magic v

(* transformer un tableau de bits en liste de types concrets *)
let list_of_mask mask =
  let list = ref []
  in
  for i=0 to 30 do
    if ((mask lsr i) land 1)=1 then
      list := i::(!list)
  done;
  Obj.magic (!list)
;;

(* transformer une liste de types concrets en un tableau de bits *)
let mask_of_list l =
  let rec iter list mask =
    match list with [] -> mask
    | i :: tail -> iter tail (mask lor (1 lsl (Obj.magic i)))
  in
  iter l 0 
;;  
  (*
  let mask = ref 0
  and n = List.length l
  and (list: int list) = Obj.magic l
  in
  List.iter (function i -> mask:=(!
  !mask
  *)

open Char
open String  

  (* les cardinaux *)
let setCard8 buffer pos int = 
  unsafe_set buffer pos (unsafe_chr int)
let setCard16 buffer pos int =
  unsafe_set buffer pos (unsafe_chr int);
  unsafe_set buffer (pos+1) (unsafe_chr (int asr 8))

let setCard32 buffer pos int =
  unsafe_set buffer pos (unsafe_chr int);
  unsafe_set buffer (pos+1) (unsafe_chr (int asr 8));
  unsafe_set buffer (pos+2) (unsafe_chr (int asr 16));
  unsafe_set buffer (pos+3) (unsafe_chr (int asr 24))
  (*L*)
external char_code : char -> int = "%identity"
  
let getCard8 buffer pos = char_code (unsafe_get buffer pos)
let getCard16 buffer pos =
  char_code (unsafe_get buffer pos) lor 
    ((char_code (unsafe_get buffer (pos+1))) lsl 8)
let getCard32 buffer pos =
  (getCard16 buffer pos) lor ((getCard16 buffer (pos+2)) lsl 16)

let getTime buffer pos = 
  Obj.magic (getCard16 buffer (pos+2),getCard16 buffer pos)

let setTime buffer pos t =
  let (t1,t2) = Obj.magic t in
  setCard16 buffer pos t2;
  setCard16 buffer (pos+2) t1
  
(* les entiers signes *)
let setInt8 = setCard8   (* les memes que les cardinaux *)
let setInt16 = setCard16
let setInt32 = setCard32
(*L*)
let getInt8 buffer pos =
  let v =char_code (unsafe_get buffer pos)
  in
  v lor ((-(v lsr 7)) lsl 8)

let getInt16 buffer pos =
  let v=getCard16 buffer pos
  in 
  v lor ((-(v lsr 15)) lsl 16)
let getInt32 buffer pos=
  let v=getCard16 buffer pos
  in 
  v lor ((-(v lsr 31)) lsl 32)
  

(* les types concrets *)
let setEnum8 buffer pos valeur = 
  unsafe_set buffer pos (unsafe_chr (int_of_enum valeur))
let setEnum16 buffer pos value =
  setCard16 buffer pos (int_of_enum value)
let setEnum32 buffer pos value =
  setCard32 buffer pos (int_of_enum value)
(*L*)
let getEnum8 buffer pos = enum_of_int (char_code (unsafe_get buffer pos))
let getEnum16 buffer pos = enum_of_int (getCard16 buffer pos)
let getEnum32 buffer pos = enum_of_int (getCard32 buffer pos)

(* les chaines *)
(* pour le debogage
let string_blit src src_x dst dst_x len =
  print_string "String.blit :\n";
  print_string "src src_x dst dst_x len";
  print_int (String.length src); print_char ' ';
  print_int src_x; print_char ' ';
  print_int (String.length dst); print_char ' ';
  print_int dst_x; print_char ' ';
  print_int len; print_char '\n';
  String.blit src src_x dst dst_x len
*)
let string_blit = String.blit;;
let setString buffer pos s =
  string_blit s 0 buffer pos (String.length s)

let setSubString buffer pos s src_pos len =
  string_blit s 0 buffer pos len

let getString = String.sub;;

let strLen n = 
  if n = 0 then 0 else ((n-1) lsr 2)+1

(* lire une liste de card32 *)
let rec get_card32_list b pos n =
  if n = 0 then
    []
  else
    (getCard32 b pos)
    ::
    (get_card32_list b (pos+4) (n-1))

(* lire une liste de card32 *)
let rec get_enum32_list b pos n =
  if n = 0 then
    []
  else
    (getEnum32 b pos)
    ::
    (get_enum32_list b (pos+4) (n-1))

(* ecrire une liste de card32 *)
let rec set_int32_list b pos = function
    [] -> ()
  | h::tl ->
      setCard32 b pos h;
      set_int32_list b (pos+4) tl

let rec set_enum32_list b pos = function
    [] -> ()
  | h::tl ->
      setEnum32 b pos h;
      set_enum32_list b (pos+4) tl



let set_card32_array b pos array =
  for i = 0 to (Array.length array)-1 do
    setCard32 b (pos+4*i) array.(i)
  done

let get_card32_array b pos n =
  let array = Array.create n 0
  in
  for i = 0 to n-1 do
    array.(i) <- getCard32 b (pos+(4*i))
  done;
  array

let newString len = String.create (len*4)
    
let rec set_str_list b pos = function
    [] -> ()
  | h::tl ->
      let n = String.length h
      in
      setCard8 b pos n;
      setString b (pos+1) h;
      set_str_list b (pos+n+1) tl
;;

let rec get_str_list b pos n =
  if n=0 then
    []
  else
  let len = getCard8 b pos
  in
  (String.sub b (pos+1) len)
  ::
  (get_str_list b (pos+len+1) (n-1))
;;

let setString16 buffer pos tab =
  let n = Array.length tab
  in
  for i=0 to n-1 do
    setCard16 buffer (pos+2*i) tab.(i)
  done
;;

(*

   Les types X extraits

*)

let xopcode opcode = xopcodes.(Obj.magic (opcode : requestOpcode))
let xerror error = xerrors.(Obj.magic (error : errorCode))

let print_xerror error =
  Printf.printf "Warning: X Error: %s for request %s\n" (xerror error.err_code) (xopcode error.err_request_code);
  Printf.printf "    Resource: %d  Minor code: %d" error.err_resourceid error.err_minor_code;
  print_newline ()

let to_string e =
  match e with
    XError error ->
      Printf.sprintf "{ %s for request %s Resource: %d  Minor code: %d}"
        (xerror error.err_code) (xopcode error.err_request_code) 
      error.err_resourceid error.err_minor_code
  | _ -> raise Not_found
      
(*
      let _ = Printexc.install 
    (XError { 
      err_resourceid = 0;
      err_serial = 0;
      err_code = Success;
      err_request_code = UnusedOpcode;
      err_minor_code = 0 }) to_string
*)  

let xerror_flag = ref false  
let getXError b =
  let e = {
      err_code = getEnum8 b 1;
      err_serial = getCard16 b 2;
      err_resourceid = getCard32 b 4;
      err_minor_code = getCard16 b 8;
      err_request_code = getEnum8 b 10
    } in
  if !xerror_flag then print_xerror e;
  e

let getXPixmapFormat buffer pos =
  {
    pxf_depth = getCard8 buffer pos;
    pxf_bits_per_pixel = getCard8 buffer (pos+1);
    pxf_scanline_pad = getCard8 buffer (pos+2)
  }

let getXVisualType buffer pos =
  {
    vsl_id =  getCard32 buffer pos;
    vsl_class =  getEnum8 buffer (pos+4);
    vsl_bits_per_rgb = getCard8 buffer (pos+5);
    vsl_colormap_entries = getCard16 buffer (pos+6);
    vsl_red_mask = getCard32 buffer (pos+8);
    vsl_green_mask = getCard32 buffer (pos+12);
    vsl_blue_mask = getCard32 buffer (pos+16)
  }

let rec getXVisualTypeList buffer pos n =
  if n=0 then
    []
  else
    (getXVisualType buffer pos)
    ::(getXVisualTypeList buffer (pos+24) (n-1))

let getXDepth buffer pos =
  ({  
      dpf_depth = getCard8 buffer pos;
      dpf_visuals = getXVisualTypeList buffer (pos+8) (getCard16 buffer (pos+2))
    },pos+8+24*(getCard16 buffer (pos+2)))

let rec getXPixmapFormatList buffer pos n =
  if n=0 then
    []
  else
    (getXPixmapFormat buffer pos)
    ::(getXPixmapFormatList buffer (pos+8) (n-1))

let rec getXDepthList buffer pos n =
  if n=0 then
    ([],pos)
  else
  let (h,nextpos) = getXDepth buffer pos
  in
  let (tail,nextpos) = getXDepthList buffer nextpos (n-1)
  in
  (h::tail,nextpos)

let getXScreen buffer pos =
  let (list,nextpos) = getXDepthList buffer (pos+40)
    (getCard8 buffer (pos+39))
  in
  ({  
      scr_root = getEnum32 buffer pos;
      scr_default_colormap = getEnum32 buffer (pos+4);
      scr_white_pixel = getEnum32 buffer (pos+8);
      scr_black_pixel = getEnum32 buffer (pos+12);
      scr_current_input_mask = list_of_mask (getCard32 buffer (pos+16));
      scr_width =  getCard16 buffer (pos+20);
      scr_height = getCard16 buffer (pos+22);
      scr_width_mm = getCard16 buffer (pos+24);
      scr_height_mm = getCard16 buffer (pos+26);
      scr_min_installed_colormap = getCard16 buffer (pos+28);
      scr_max_installed_colormap = getCard16 buffer (pos+30);
      scr_root_visual_id = getEnum32 buffer (pos+32);
      scr_backing_stores =  getEnum8 buffer (pos+36);
      scr_save_unders =  getEnum8 buffer (pos+37);
      scr_root_depth =  getCard8 buffer (pos+38);
      scr_depths = list
    },nextpos)
;;

let rec getXScreenList buffer pos n =
  if n=0 then
    []
  else
  let (h,nextpos) = getXScreen buffer pos
  in
  h::(getXScreenList buffer nextpos (n-1))
;;

let getXServerInfo b sock maj min screen_num server_name =
  let (v,n,ns) = (getCard16 b 16,getCard8 b 21,getCard8 b 20)
  in
  {
    socket = sock;
    serial_out= 0;
    serial_in= 0;
    event_queue = Equeue.create (Obj.magic 0);
    wrappers_queue = Xfifo.create (Obj.magic 0);
    
    last_resource_id = 0;
    resource_incr = 
    (let decalage = ref 0
      and mask = ref (getCard32 b 8)
      in
      while (!mask land 1)=0 do
        mask := (!mask) lsr 1;
        incr decalage
      done;
      1 lsl (!decalage));
    dpy_broken = (fun () -> 
        prerr_endline "Broken connection to server.";
        exit 1);
    dpy_key_bindings = [];
    dpy_proto_major = maj;
    dpy_proto_minor = min;
    dpy_proto_release = getCard32 b 0;
    resource_base = getCard32 b 4;
    resource_mask = getCard32 b 8;
    dpy_motion_buffer_size = getCard32 b 12;
    dpy_max_request_length = getCard16 b 18;
    dpy_screen_number = ns;
    dpy_screen_default =  screen_num;
    dpy_pixmap_format_number = getCard8 b 21;
    dpy_image_byte_order =  getEnum8 b 22;
    dpy_bitmap_format_bit_order =  getEnum8 b 23;
    dpy_bitmap_format_scanline_unit = getCard8 b 24;
    dpy_bitmap_format_scanline_pad = getCard8 b 25;
    dpy_min_keycode =  getCard8 b 26;
    dpy_max_keycode =  getCard8 b 27;
    dpy_vendor =  getString b 32 v;
    dpy_keysyms = [||];
    dpy_modifiermap = [||];
    dpy_keysyms_per_keycode = 0;
    dpy_lock_meaning = 0;
    dpy_mode_switch = 0;
    dpy_num_lock = 0;
    dpy_pixmap_formats = getXPixmapFormatList b (32+4*(strLen v)) n ;  
    dpy_roots = Array.of_list 
      (getXScreenList b (32+4*(strLen v)+8*n) ns);
    dpy_server_name = server_name;
    dpy_extensions = [];
  }
  
let getWindow = getEnum32
let setWindow = setEnum32
let getFont = getEnum32
let setFont = setEnum32
let getColormap = getEnum32
let setColormap = setEnum32
let getAtom = getEnum32
let setAtom = setEnum32
