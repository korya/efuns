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

(* NOTE: This module is completely architecture dependent. Indeed, we expect
sizeof(int) = 4 and standard byte ordering. *) 

open Options
open Unix
open Xtypes
open Gwml
open Stdconfig

  
let config_info = ref []
  
(*
This file try to implement module communication as done by Fvwm.
*)
let all = 0

type protocol =
  Fvwm1
| Fvwm2
| Afterstep
  
(* The list of pipes to communicate with fvwm_modules *)
type fvwm_module = {
    name : string;
    write_pipe : file_descr;
    read_pipe : file_descr;
    proto : protocol;
    mutable pipe_mask : int;
    mutable pipe_queue : int list;
    mutable pipe_on : bool;
    mutable module_mask : int;
  }

let module_path = ref ([]: string list)
  
let fvwm_modules = ref []

let string_of_file_descr (file_descr : file_descr) = 
  string_of_int (Obj.magic file_descr)
  
type context = 
  C_NO_CONTEXT
| C_WINDOW
| C_TITLE
| C_ICON
| C_ROOT
| C_FRAME
| C_SIDEBAR
| C_L1
| C_L2
| C_L3
| C_L4
| C_L5
| C_R1
| C_R2
| C_R3
| C_R4
| C_R5
  
let int_of_context = function
    C_NO_CONTEXT -> 0
  | C_WINDOW -> 1
  | C_TITLE -> 2
  | C_ICON -> 4
  | C_ROOT -> 8
  | C_FRAME -> 16
  | C_SIDEBAR -> 32
  | C_L1 -> 64
  | C_L2 -> 128
  | C_L3 -> 256
  | C_L4 -> 512
  | C_L5 -> 1024
  | C_R1 -> 2048
  | C_R2 -> 4096
  | C_R3 -> 8192
  | C_R4 -> 16384
  | C_R5 -> 32768
    
    
    (* module-to-fvwm :
    window-id or None (32 bits), size (int), message, 0 (stop) or 1 (continue)
  Send_WindowList, Send_ConfigInfo and Set_Mask
    *)
  
type packet_type =
  M_NEW_PAGE
| M_NEW_DESK
| M_CONFIGURE_WINDOW 
| M_ADD_WINDOW
| M_RAISE_WINDOW 
| M_LOWER_WINDOW
| M_DESTROY_WINDOW
| M_MAP
| M_DEICONIFY
| M_FOCUS_CHANGE
| M_ICONIFY
| M_ICON_LOCATION
| M_WINDOW_NAME
| M_ICON_NAME
| M_RES_CLASS
| M_RES_NAME
| M_END_WINDOWLIST
| M_ERROR
| M_CONFIG_INFO 
| M_END_CONFIG_INFO
| M_ICON_FILE
| M_DEFAULTICON 
| M_FUNCTION_END
| M_TITLE_ICON
| M_SCROLLREGION
| M_TOGGLE_PAGING
| M_SHADE
| M_UNSHADE       
| M_LOCKONSEND    
| M_NEW_BACKGROUND
  
let print_packet packet =
  match packet with
| M_NEW_PAGE -> "M_NEW_PAGE"
| M_NEW_DESK -> "M_NEW_DESK"
| M_CONFIGURE_WINDOW -> "M_CONFIGURE_WINDOW" 
| M_ADD_WINDOW -> "M_ADD_WINDOW"
| M_RAISE_WINDOW -> "M_RAISE_WINDOW" 
| M_LOWER_WINDOW -> "M_LOWER_WINDOW"
| M_DESTROY_WINDOW -> "M_DESTROY_WINDOW"
| M_MAP -> "M_MAP"
| M_DEICONIFY -> "M_DEICONIFY"
| M_FOCUS_CHANGE -> "M_FOCUS_CHANGE"
| M_ICONIFY -> "M_ICONIFY"
| M_ICON_LOCATION -> "M_ICON_LOCATION"
| M_WINDOW_NAME -> "M_WINDOW_NAME"
| M_ICON_NAME -> "M_ICON_NAME"
| M_RES_CLASS -> "M_RES_CLASS"
| M_RES_NAME -> "M_RES_NAME"
| M_END_WINDOWLIST -> "M_END_WINDOWLIST"
| M_ERROR -> "M_ERROR"
| M_CONFIG_INFO -> "M_CONFIG_INFO" 
| M_END_CONFIG_INFO -> "M_END_CONFIG_INFO"
| M_ICON_FILE -> "M_ICON_FILE"
| M_DEFAULTICON -> "M_DEFAULTICON" 
| M_FUNCTION_END -> "M_FUNCTION_END"
| M_TITLE_ICON -> "M_TITLE_ICON"
| M_SCROLLREGION -> "M_SCROLLREGION"
| M_TOGGLE_PAGING -> "M_TOGGLE_PAGING"
| M_SHADE -> "M_SHADE"
| M_UNSHADE -> "M_UNSHADE"       
| M_LOCKONSEND -> "M_LOCKONSEND"    
| M_NEW_BACKGROUND -> "M_NEW_BACKGROUND"
  
let int_to_packet_fvwm2 int =
  match int with
    1 -> M_NEW_PAGE
  | 2 -> M_NEW_DESK
  | 4 -> M_ADD_WINDOW
  | 8 -> M_RAISE_WINDOW
  | 16 -> M_LOWER_WINDOW
  | 32 -> M_CONFIGURE_WINDOW
  | 64 -> M_FOCUS_CHANGE
  | 128 -> M_DESTROY_WINDOW
  | 256 -> M_ICONIFY
  | 512 -> M_DEICONIFY
  | 1024 -> M_WINDOW_NAME
  | 2048 -> M_ICON_NAME
  | 4096 -> M_RES_CLASS
  | 8192 -> M_RES_NAME
  | 16384 -> M_END_WINDOWLIST
  | 32768 -> M_ICON_LOCATION
  | 65536 -> M_MAP
  | 131072 -> M_ERROR
  | 262144 -> M_CONFIG_INFO
  | 524288 -> M_END_CONFIG_INFO
  | 1048576 -> M_ICON_FILE
  | 2097152 -> M_DEFAULTICON 
  | 4194304 -> M_FUNCTION_END       
  | 8388608 -> M_TITLE_ICON         
  | 16777216 -> M_SCROLLREGION       
  | _ -> failwith "packet not understood"  
      
let int_to_packet_fvwm1 int =
  match int with
    1 -> M_TOGGLE_PAGING
  | 2 -> M_NEW_PAGE
  | 4 -> M_NEW_DESK
  | 8 -> M_ADD_WINDOW
  | 16 -> M_RAISE_WINDOW
  | 32 -> M_LOWER_WINDOW
  | 64 -> M_CONFIGURE_WINDOW
  | 128 -> M_FOCUS_CHANGE
  | 256 -> M_DESTROY_WINDOW
  | 512 -> M_ICON_FILE
  | 1024 -> M_DEICONIFY
  | 2048 -> M_WINDOW_NAME
  | 4096 -> M_ICON_NAME
  | 8192 -> M_RES_CLASS
  | 16384 -> M_RES_NAME
  | 32768 -> M_END_WINDOWLIST
  | 65536 -> M_ICON_LOCATION
  | 131072 -> M_MAP
  | _ -> failwith "packet not understood"
      
let packet_to_int_fvwm2 int =
  match int with
    M_NEW_PAGE -> 1 lsl 0
  | M_NEW_DESK -> 1 lsl 1
  | M_ADD_WINDOW -> 1 lsl 2
  | M_RAISE_WINDOW -> 1 lsl 3
  | M_LOWER_WINDOW -> 1 lsl 4
  | M_CONFIGURE_WINDOW -> 1 lsl 5
  | M_FOCUS_CHANGE -> 1 lsl 6
  | M_DESTROY_WINDOW -> 1 lsl 7
  | M_ICONIFY -> 1 lsl 8
  | M_DEICONIFY -> 1 lsl 9
  | M_WINDOW_NAME -> 1 lsl 10
  | M_ICON_NAME -> 1 lsl 11
  | M_RES_CLASS -> 1 lsl 12
  | M_RES_NAME -> 1 lsl 13
  | M_END_WINDOWLIST -> 1 lsl 14
  | M_ICON_LOCATION -> 1 lsl 15
  | M_MAP -> 1 lsl 16
  | M_ERROR -> 1 lsl 17
  | M_CONFIG_INFO -> 1 lsl 18
  | M_END_CONFIG_INFO -> 1 lsl 19
  | M_ICON_FILE -> 1 lsl 20
  | M_DEFAULTICON -> 1 lsl 21
  | M_FUNCTION_END -> 1 lsl 22
  | M_TITLE_ICON -> 1 lsl 23
  | M_SCROLLREGION -> 1 lsl 24
  | _ -> failwith "Not implemented in Fvwm mode"

let packet_to_int_afterstep int =
  match int with
    M_TOGGLE_PAGING       -> (1 lsl 0)
  | M_NEW_PAGE           -> (1 lsl 1)
  | M_NEW_DESK           -> (1 lsl 2)
  | M_ADD_WINDOW         -> (1 lsl 3)
  | M_RAISE_WINDOW       -> (1 lsl 4)
  | M_LOWER_WINDOW       -> (1 lsl 5)
  | M_CONFIGURE_WINDOW   -> (1 lsl 6)
  | M_FOCUS_CHANGE       -> (1 lsl 7)
  | M_DESTROY_WINDOW     -> (1 lsl 8)
  | M_ICONIFY            -> (1 lsl 9)
  | M_DEICONIFY          -> (1 lsl 10)
  | M_WINDOW_NAME        -> (1 lsl 11)
  | M_ICON_NAME          -> (1 lsl 12)
  | M_RES_CLASS          -> (1 lsl 13)
  | M_RES_NAME           -> (1 lsl 14)
  | M_END_WINDOWLIST     -> (1 lsl 15)
  | M_ICON_LOCATION      -> (1 lsl 16)
  | M_MAP                -> (1 lsl 17)
  | M_SHADE              -> (1 lsl 18)
  | M_UNSHADE            -> (1 lsl 19)
  | M_LOCKONSEND         -> (1 lsl 20)
  | M_NEW_BACKGROUND     -> (1 lsl 21)
  | _ ->  failwith "Not implemented in Afterstep mode"

            
let packet_to_int_fvwm1 int =
  match int with
    M_TOGGLE_PAGING -> 1
  | M_NEW_PAGE -> 2
  | M_NEW_DESK -> 4
  | M_ADD_WINDOW -> 8
  | M_RAISE_WINDOW -> 16
  | M_LOWER_WINDOW -> 32
  | M_CONFIGURE_WINDOW -> 64
  | M_FOCUS_CHANGE -> 128
  | M_DESTROY_WINDOW -> 256
  | M_ICON_FILE -> 512
  | M_DEICONIFY -> 1024
  | M_WINDOW_NAME -> 2048
  | M_ICON_NAME -> 4096
  | M_RES_CLASS -> 8192
  | M_RES_NAME -> 16384
  | M_END_WINDOWLIST -> 32768
  | M_ICON_LOCATION -> 65536
  | M_MAP -> 131072
  | _ -> failwith "packet not understood"
            
let int_to_packet m int =
  match m.proto with
    Fvwm1 -> int_to_packet_fvwm1 int
  | _ -> int_to_packet_fvwm2 int
    
let packet_to_int m int =
  match m.proto with
    Fvwm1 -> packet_to_int_fvwm1 int
  | Afterstep -> packet_to_int_afterstep int
  | Fvwm2 -> packet_to_int_fvwm2 int
      
let long = Sys.word_size / 8
let int = 4
let buffer = String.create (255 * long)
  
let kill_module m =
  close m.write_pipe;
  close m.read_pipe;
  fvwm_modules := Utils.list_removeq !fvwm_modules m;
  Concur.Thread.remove_reader m.read_pipe

  

let start = 0xffffffff
let hsize m = 
  match m.proto with
    Fvwm2 -> 4
  | _ -> 3

  (* These two functions are completely hardware dependent *)
let setInt pos n =
  Xbuffer.setEnum32 buffer (pos * int) n
  
let setLong pos n = 
  Xbuffer.setEnum32 buffer (pos * long) n;
  if long > 4 then Xbuffer.setEnum32 buffer (pos * long + 4) 0

let getInt pos =
  Xbuffer.getEnum32 buffer (pos * int)
  
let getLong pos =
  let n = Xbuffer.getEnum32 buffer (pos * long) in
  if long > 4 then 
    n + (Xbuffer.getEnum32 buffer (pos * long + 4) lsl 32)
  else n
    
let setString pos s =
  let len = String.length s in
  let pos = pos * long in
  String.blit s 0 buffer pos len;
  buffer.[pos+len] <- '\000';
  buffer.[pos+len+1] <- '\000';
  buffer.[pos+len+2] <- '\000';
  buffer.[pos+len+3] <- '\000'

let send_buffer m packet sz =
  let int = packet_to_int m packet in
  let sz = sz + hsize m in
  if int land m.module_mask <> 0 then begin
      setLong 0 start;
      setLong 1 int;
      setLong 2 sz;
      (match m.proto with
          Fvwm1 | Afterstep -> ()
        | Fvwm2 -> setLong 3 !Eloop.event_time);
      let sz = sz * long in
      let rec iter pos size =
        let len = Unix.write m.write_pipe buffer pos size in
        if len = 0 then (kill_module m; raise End_of_file)
        else
        if len = size then  ()
        else iter (pos+len) (size-len)
      in
      iter 0 sz
    end
(* FAKE desktops *)
  
let send_NewDesk m w =
  let pos = hsize m in
  setLong 4 pos;
  send_buffer m M_NEW_DESK 1
  
let send_NewPage m w =
  let sg = w.w_top.w_parent.w_geometry in
  let (vx,vy) = !virtual_manager#current_position w in
  let pos = hsize m in
  setLong pos vx;
  setLong (pos+1) vy;
  setLong (pos+2) 0; (* desktop *)
  setLong (pos+3) (max vx (sg.width * 3)); (* max vx *)
  setLong (pos+4) (max vy (sg.height * 3));  
  send_buffer m M_NEW_PAGE 5
  
let send_FocusChange m w =
  (* args *)
  let s = w.w_screen in
  let sw = w.w_top.w_parent in
  let pos = hsize m in
  setLong (pos+3) (color_make w !!active_foreground);
  setLong (pos+4) (color_make w !!active_background);
  if s.s_focus_wob == sw then 
    begin
      setLong pos 0;
      setLong (pos+1) 0;
      setLong (pos+2) 0;
    end
  else
    begin
      let fw = s.s_focus_wob in
      setLong pos w.w_window;
      setLong (pos+1) w.w_top.w_window;
      setLong (pos+2) w.w_window;      
    end;
  send_buffer m M_FOCUS_CHANGE 5
    
let send_EndWindowList m w = send_buffer m M_END_WINDOWLIST 0    
let send_EndConfigInfo m w = send_buffer m M_END_CONFIG_INFO 0

let send_window m (c,w) packet =
  let pos = hsize m in
  setLong pos (window_to_id c.c_window);
  setLong (pos+1) (window_to_id w.w_top.w_window);
  setLong (pos+2) (window_to_id c.c_window);
  send_buffer m packet 3
 
let send_config m (c,w) packet =
  let pos = hsize m in
  setLong pos (window_to_id c.c_window);
  setLong (pos+1) (window_to_id w.w_top.w_window);
  setLong (pos+2) (window_to_id c.c_window);
  let tw = w.w_top in
  let tg = tw.w_geometry in
  setLong (pos+3) tg.x;
  setLong (pos+4) tg.y;
  setLong (pos+5) tg.width;
  setLong (pos+6) tg.height;
  setLong (pos+7) 0; (* desk *)
  setLong (pos+8) 0; (* flags *)
  setLong (pos+9) 0; (*  title-height *)
  setLong (pos+10) 0; (* border-width *)
  setLong (pos+11) 0; (* base-width *)
  setLong (pos+12) 0; (* base-height *)
  setLong (pos+13) 1; (* resize-w-inc *)
  setLong (pos+14) 1; (* resize-h-inc *)
  setLong (pos+15) 0; (* min-width *)
  setLong (pos+16) 0; (* min-height *)
  setLong (pos+17) 1; (* max-w-inc *)
  setLong (pos+18) 1; (* max-h-inc *)
  setLong (pos+19) 0; (* icon-label-WID *)
  setLong (pos+20) 0; (* icon-pixmap-WID *)
  setLong (pos+21) 1; (* win-grav *)
  setLong (pos+22) 0; (* text-color *)
  setLong (pos+23) 1; (* border-color *)
  send_buffer m packet (match m.proto with
      Fvwm2 -> 24 | Fvwm1 -> 22 | Afterstep -> 24)
    
let send_string m (c,w) packet s =
  let pos = hsize m in
  setLong pos (window_to_id c.c_window);
  setLong (pos+1) (window_to_id w.w_top.w_window);
  setLong (pos+2) (window_to_id c.c_window);
  let len = String.length s in
  let sz = (len / 4) + 4 in
  setString (pos+3) s;
  send_buffer m packet sz
      
let send_ConfigInfo m w =
  let pos = hsize m in
  setLong pos 0;
  setLong (pos+1) 0;
  setLong (pos+2) 0;
  
  List.iter (fun s ->
      let len = String.length s in
      let sz = (len / 4) + 4 in
      setString (pos+3) s;
      send_buffer m M_CONFIG_INFO sz
  ) (
    (Printf.sprintf "PixmapPath %s" (Utils.path_to_string !!icon_path))::
    (Printf.sprintf "IconPath %s" (Utils.path_to_string !!icon_path)) ::
    !config_info
  );
  send_EndConfigInfo m w

let send_WindowName m (c,w) =
  send_string m (c,w) M_WINDOW_NAME c.c_name
  
let send_IconName m (c,w) =
  send_string m (c,w) M_ICON_NAME c.c_icon_name

let send_ResClass m (c,w) =
  send_string m (c,w) M_RES_CLASS (fst c.c_class)

let send_ResName m (c,w) =
  send_string m (c,w) M_RES_NAME (snd c.c_class)

  (*
let send_IconFile m (c,w) =
  send_string m (c,w) M_ICON_NAME ()
  *)
  
let send_WindowList m w =
  try
  send_NewDesk m w;
  send_NewPage m w;
  send_FocusChange m w;
  (*   SendName( *Module,M_DEFAULTICON,0,0,0,Scr.DefaultIcon); *)
  let s = w.w_screen in
  Wintbl.iter (fun win (c,w) ->
      if win = c.c_window then begin
          send_config m (c,w) M_CONFIGURE_WINDOW;
          send_WindowName m (c,w);
          send_IconName m (c,w);
  (*
           SendName( *Module,M_ICON_FILE,t->w,t->frame,
  (unsigned long)t,t->icon_bitmap_file);
          *)
          send_ResClass m (c,w);
          send_ResName m (c,w);
(*             
            SendPacket( *Module,M_ICONIFY,7,t->w,t->frame,
                       (unsigned long)t,
                       t->icon_x_loc,t->icon_y_loc,
                       t->icon_w_width, 
                       t->icon_w_height+t->icon_p_height);
          if((t->flags & ICONIFIED) && (t->flags & ICON_UNMAPPED))
            SendPacket( *Module,M_ICONIFY,7,t->w,t->frame,
                       (unsigned long)t,0,0,0,0);
          if (t->title_icon != NULL)
            SendPacket( *Module, M_TITLE_ICON, 6,
                       t->w, /* Watch Out ! : I reduced the set of infos... */
                       t->title_icon->picture,
                       t->title_icon->mask,
                       t->title_icon->width,
                       t->title_icon->height,
          t->title_icon->depth, 0);
            *)
          end
  ) s.s_clients;
    send_EndWindowList m w
  with
    e -> Printf.printf "Name %s" (Utils.printexn e);
      print_newline (); raise e

let broadcast_to_fvwm_modules (c,w,ev) =
  let s = c.c_screen in
  List.iter (fun m ->
      match ev with
      | AddClient -> 
          send_config m (c,w) M_ADD_WINDOW;
          send_WindowName m (c,w);
          send_IconName m (c,w);
          send_ResClass m (c,w);
          send_ResName m (c,w);
      | RemoveClient -> 
          send_window m (c,w) M_DESTROY_WINDOW
      | ClientResize -> ()
      | ClientMove -> ()
      | ClientUnmap -> ()
      | ClientMap -> 
          send_window m (c,w) M_MAP
      | ClientIconify -> ()
      | ClientDeiconify -> ()
      | ClientPropertyChange atom -> ()
      | ClientColormap install -> ()
      | ClientFocus enter -> ()
  ) !fvwm_modules    
  
    
let _ =
  broadcast_targets := broadcast_to_fvwm_modules :: !broadcast_targets