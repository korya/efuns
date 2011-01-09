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

let (--) p = p := !p-1
let (-=) a b = a := !a - b
let (+=) a b = a := !a + b
let (|=) a b = a := (!a lor b)
let (lsr) a b = a lsr b
let (lsl) a b = a lsl b


let allMods =  shiftMask lor lockMask lor controlMask lor  
                 mod1Mask lor mod2Mask lor mod3Mask lor mod4Mask lor mod5Mask

exception EnableToGetKeysyms
exception EnableToGetModifierMapping

let uKeysymToModifiers display (ks : keySym) =
  let mods = ref 0 in
  let keypermod = Array.length display.dpy_modifiermap.(0) in
    for k = 0 to display.dpy_max_keycode - display.dpy_min_keycode do
      for l = 0 to display.dpy_keysyms_per_keycode - 1  do
        if display.dpy_keysyms.(k).(l) = ks then
          let keycode = k+display.dpy_min_keycode in
            for m = 0 to 7 do
              for j = 0 to keypermod-1 do
                if keycode = display.dpy_modifiermap.(m).(j) then 
                  mods |=  (1 lsl m)
              done;
            done;
      done;
    done;
    !mods
      
let computeMaskFromKeyTrans display p =
  p.key_state <- anyModifier;
  for i=0 to p.key_mlen - 1 do
    p.key_state <- p.key_state lor
    (uKeysymToModifiers display p.key_modifiers.(i))
  done; 
  p.key_state <- p.key_state land allMods


let rec recComputeMaskFromKeyTrans display =
  function
      [] -> ()
    | p :: list ->
        computeMaskFromKeyTrans display p;
        recComputeMaskFromKeyTrans display list

let convertCase sym =
  let (lower,upper) = (ref sym,ref sym)
  in
    begin
      match sym lsr 8 with
          0 ->
	    if ((sym >= XK.xk_A) && (sym <= XK.xk_Z)) then
	      lower += (XK.xk_a - XK.xk_A)
            else if ((sym >= XK.xk_a) && (sym <= XK.xk_z)) then
              upper -= (XK.xk_a - XK.xk_A)
            else if ((sym >= XK.xk_Agrave) && (sym <= XK.xk_Odiaeresis)) then
              lower += (XK.xk_agrave - XK.xk_Agrave)
            else if ((sym >= XK.xk_agrave) && (sym <= XK.xk_odiaeresis)) then
              upper -= (XK.xk_agrave - XK.xk_Agrave)
	    else if ((sym >= XK.xk_Ooblique) && (sym <= XK.xk_Thorn)) then
	      lower += (XK.xk_oslash - XK.xk_Ooblique)
	    else if ((sym >= XK.xk_oslash) && (sym <= XK.xk_thorn)) then
	      upper -= (XK.xk_oslash - XK.xk_Ooblique)
                                             | 1 ->
  (* Assume the KeySym is a legal value (ignore discontinuities) *)
  if (sym = XK.xk_Aogonek) then
    lower := XK.xk_aogonek
  else if (sym >= XK.xk_Lstroke && sym <= XK.xk_Sacute) then
    lower += (XK.xk_lstroke - XK.xk_Lstroke)
  else if (sym >= XK.xk_Scaron && sym <= XK.xk_Zacute) then
    lower += (XK.xk_scaron - XK.xk_Scaron)
  else if (sym >= XK.xk_Zcaron && sym <= XK.xk_Zabovedot) then
    lower += (XK.xk_zcaron - XK.xk_Zcaron)
  else if (sym = XK.xk_aogonek) then
    upper := XK.xk_Aogonek
  else if (sym >= XK.xk_lstroke && sym <= XK.xk_sacute) then
    upper -= (XK.xk_lstroke - XK.xk_Lstroke)
  else if (sym >= XK.xk_scaron && sym <= XK.xk_zacute) then
    upper -= (XK.xk_scaron - XK.xk_Scaron)
  else if (sym >= XK.xk_zcaron && sym <= XK.xk_zabovedot) then
    upper -= (XK.xk_zcaron - XK.xk_Zcaron)
  else if (sym >= XK.xk_Racute && sym <= XK.xk_Tcedilla) then
    lower += (XK.xk_racute - XK.xk_Racute)
  else if (sym >= XK.xk_racute && sym <= XK.xk_tcedilla) then
    upper -= (XK.xk_racute - XK.xk_Racute);
  | 2 ->
      (* Assume the KeySym is a legal value (ignore discontinuities) *)
      if (sym >= XK.xk_Hstroke && sym <= XK.xk_Hcircumflex) then
	lower += (XK.xk_hstroke - XK.xk_Hstroke)
      else if (sym >= XK.xk_Gbreve && sym <= XK.xk_Jcircumflex) then
	lower += (XK.xk_gbreve - XK.xk_Gbreve)
      else if (sym >= XK.xk_hstroke && sym <= XK.xk_hcircumflex) then
        upper -= (XK.xk_hstroke - XK.xk_Hstroke)
      else if (sym >= XK.xk_gbreve && sym <= XK.xk_jcircumflex) then
        upper -= (XK.xk_gbreve - XK.xk_Gbreve)
      else if (sym >= XK.xk_Cabovedot && sym <= XK.xk_Scircumflex) then
        lower += (XK.xk_cabovedot - XK.xk_Cabovedot)
      else if (sym >= XK.xk_cabovedot && sym <= XK.xk_scircumflex) then
         upper -= (XK.xk_cabovedot - XK.xk_Cabovedot)
   | 3 ->
       (* Assume the KeySym is a legal value (ignore discontinuities) *)
       if (sym >= XK.xk_Rcedilla && sym <= XK.xk_Tslash) then
	 lower += (XK.xk_rcedilla - XK.xk_Rcedilla)
       else if (sym >= XK.xk_rcedilla && sym <= XK.xk_tslash) then
	 upper -= (XK.xk_rcedilla - XK.xk_Rcedilla)
       else if (sym = XK.xk_ENG) then
	 lower := XK.xk_eng
       else if (sym = XK.xk_eng) then
	 upper := XK.xk_ENG
       else if (sym >= XK.xk_Amacron && sym <= XK.xk_Umacron) then
	 lower += (XK.xk_amacron - XK.xk_Amacron)
       else if (sym >= XK.xk_amacron && sym <= XK.xk_umacron) then
	 upper -= (XK.xk_amacron - XK.xk_Amacron)
   | 6 ->
       (* Assume the KeySym is a legal value (ignore discontinuities) *)
       if (sym >= XK.xk_Serbian_DJE && sym <= XK.xk_Serbian_DZE) then
	 lower -= (XK.xk_Serbian_DJE - XK.xk_Serbian_dje)
       else if (sym >= XK.xk_Serbian_dje && sym <= XK.xk_Serbian_dze) then
	 upper += (XK.xk_Serbian_DJE - XK.xk_Serbian_dje)
       else if (sym >= XK.xk_Cyrillic_YU && sym <= XK.xk_Cyrillic_HARDSIGN) then
	 lower -= (XK.xk_Cyrillic_YU - XK.xk_Cyrillic_yu)
       else if (sym >= XK.xk_Cyrillic_yu && sym <= XK.xk_Cyrillic_hardsign) then
	 upper += (XK.xk_Cyrillic_YU - XK.xk_Cyrillic_yu)
   | _ -> ()
    end;
  (!lower,!upper)

let uKeyCodeToKeySym display keycode col =
  let per = ref display.dpy_keysyms_per_keycode in
  let col = ref col in
    if ((!col < 0) || ((!col >= !per) && (!col > 3)) ||
	(keycode < display.dpy_min_keycode) || (keycode > display.dpy_max_keycode))
    then
      noSymbol
    else
      let syms = display.dpy_keysyms.(keycode - display.dpy_min_keycode) in
        if (!col < 4) then
          begin
	    if (!col > 1) then
	      begin
                while (!per > 2) && (syms.(!per - 1) = noSymbol) do
		  (--) per
                done;
	        if !per < 3 then
		  col -= 2
	      end;
	    if (!per <= (!col lor 1)) || (syms.(!col lor 1) = noSymbol) then
              let (lsym,usym) = convertCase syms.(!col land (lnot 1)) in
	        if (!col land 1)=0 then lsym
	          else
                    if (usym = lsym) then noSymbol
                    else usym
            else
              syms.(!col)
         end
        else
          syms.(!col)
              
let resetModMap display =
  let map = display.dpy_modifiermap in
    display.dpy_lock_meaning <- noSymbol;
    begin
      try
        for i=0 to Array.length map.(0) - 1 do
	  for j = 0 to  display.dpy_keysyms_per_keycode - 1 do
	    let sym = uKeyCodeToKeySym display map.(2).(i) j
            in
	      if sym = XK.xk_Caps_Lock then
                begin
		  display.dpy_lock_meaning <- XK.xk_Caps_Lock;
                  raise Exit
                end
	      else if sym = XK.xk_Shift_Lock then
		display.dpy_lock_meaning <- XK.xk_Shift_Lock
	      else if sym = XK.xk_ISO_Lock then
                begin
		  display.dpy_lock_meaning <- XK.xk_Caps_Lock;
                  raise Exit
                end
          done; 
        done
      with
          Exit -> ()
    end;
    display.dpy_mode_switch <- 0;
    display.dpy_num_lock <- 0;
    for m=3 to 7 do
      for i=0 to Array.length map.(m) - 1 do
	for j = 0 to display.dpy_keysyms_per_keycode do
          let sym = uKeyCodeToKeySym display map.(m).(i) j in
	    if sym = XK.xk_Mode_switch then
		display.dpy_mode_switch <- display.dpy_mode_switch lor 1 lsl m;
	    if sym = XK.xk_Num_Lock then
		display.dpy_num_lock <- display.dpy_num_lock lor 1 lsl m
        done;
      done;
    done;
    recComputeMaskFromKeyTrans display display.dpy_key_bindings


let initModMap display =
  let map = X.getModifierMapping display
  in
    if map = [||] then
      raise EnableToGetModifierMapping;
  display.dpy_modifiermap <- map;
  if display.dpy_keysyms <> [||] then
    resetModMap display

let uKeyInitialize display =
  if display.dpy_keysyms = [||] then
    begin
      let n = display.dpy_max_keycode - display.dpy_min_keycode + 1
      in
      let map = X.getKeyboardMapping display display.dpy_min_keycode n
      in
        if map.gkm_keycodes = [||] then 
          raise EnableToGetKeysyms;
        display.dpy_keysyms <- map.gkm_keycodes;
        display.dpy_keysyms_per_keycode <- map.gkm_keysyms_per_keycode;
        if display.dpy_modifiermap <> [||] then
	  resetModMap display
    end;
  if display.dpy_modifiermap = [||] then
    initModMap display


let keycodeToKeysym display keycode col =
  uKeyInitialize display;
  uKeyCodeToKeySym display keycode col


exception Found of int * int
let keysymToKeycode display keysym =
  uKeyInitialize display;
  try
    for j=0 to display.dpy_keysyms_per_keycode - 1 do
      for i=display.dpy_min_keycode to display.dpy_max_keycode do
        if (uKeyCodeToKeySym display i j) = keysym then
          raise (Found (i,j))
      done;
    done;
    raise Not_found
  with
    Found (i,j) -> (i,j)
      
let lookupKeysym display event col =
    uKeyInitialize display;
    uKeyCodeToKeySym display 
      ((match event with
            KeyPressEvent t -> t
          | KeyReleaseEvent t -> t
          | _ -> raise (Invalid_argument "lookupKeysym : bad event")
       ).Xkey.detail) col 

let refreshKeyboardMapping display event =
    match event with
        MappingNotifyEvent e ->
          begin
            match e.Xmapping.request with
                MapModifier ->
                  display.dpy_modifiermap <- [||]
              | MapKeyboard -> 
                  display.dpy_keysyms <- [||]                  
              | _ -> assert false
          end
      | _ -> raise  (Invalid_argument "refreshKeyboardMapping : bad event")
            
let uTranslateKeySym display symbol modifiers =
  if symbol=0 then ""
  else
    (* see if symbol rebound, if so, return that string. *)
    let rec iter = function 
        [] -> 
    (* try to convert to Latin-1, handling control *)
          let hiBytes = symbol lsr 8 in
            if not (
	            ((hiBytes = 0) ||
	             ((hiBytes = 0xFF) &&
	              (((symbol >= XK.xk_BackSpace) && (symbol <= XK.xk_Clear))
                       || (symbol = XK.xk_Return) ||
	               (symbol = XK.xk_Escape) ||
	               (symbol = XK.xk_KP_Space) ||
	               (symbol = XK.xk_KP_Tab) ||
	               (symbol = XK.xk_KP_Enter) ||
	               ((symbol >= XK.xk_KP_Multiply)&&(symbol <= XK.xk_KP_9))
                       || (symbol = XK.xk_KP_Equal) ||
	               (symbol = XK.xk_Delete))))) then ""
            else
    (* if X keysym, convert to ascii by grabbing low 7 bits *)
              let c =
                if (symbol = XK.xk_KP_Space) then
	          XK.xk_space land 0x7F (* patch encoding botch *)
                else if (hiBytes = 0xFF) then
	          symbol land 0x7F
                else
	          symbol land 0xFF in
    (* only apply Control key if it makes sense, else ignore it *)
              let c =
                if (modifiers land controlMask) <> 0 then
	          if ((c >= Char.code '@') && (c < Char.code '\177')
                    || c = Char.code ' ') then c land 0x1F
                  else if (c = Char.code '2') then Char.code '\000'
	          else if (c >= Char.code '3' && c <= Char.code '7') then
                    c - (Char.code '3' - Char.code '\033')
	          else if (c = Char.code '8') then Char.code '\177'
	          else if (c = Char.code '/') then Char.code '_' land 0x1F 
                  else c
                else c
              in
                String.make 1 (Char.chr c)
      | p :: list ->
	  if ((modifiers land allMods) = p.key_state) && (symbol = p.key_key)
          then p.key_string
          else iter list
    in
      iter display.dpy_key_bindings

let rebindKeysym display keysym keysymarray str =
  uKeyInitialize display;
  let p =
    {
      key_string = String.copy str;
      key_len = String.length str;
      key_key = keysym;
      key_state = 0;
      key_mlen = Array.length keysymarray;
      key_modifiers = keysymarray
  } in
  display.dpy_key_bindings <- p :: display.dpy_key_bindings;
    computeMaskFromKeyTrans display p

(*
 * given a list of modifiers, computes the mask necessary for later matching.
 * This routine must lookup the key in the Keymap and then search to see
 * what modifier it is bound to, if any.  Sets the AnyModifier bit if it
 * can't map some keysym to a modifier.
 *)

let isKeypadKey keysym =
  (keysym >= XK.xk_KP_Space) && (keysym <= XK.xk_KP_Equal)

let isPrivateKeypadKey keysym =
  (keysym >= 0x11000000) && (keysym <= 0x1100FFFF)

let isCursorKey keysym =
  (keysym >= XK.xk_Home) && (keysym < XK.xk_Select)

let isPFKey keysym =
  (keysym >= XK.xk_KP_F1) && (keysym <= XK.xk_KP_F4)

let isFunctionKey keysym =
  (keysym >= XK.xk_F1) && (keysym <= XK.xk_F35)

let isMiscFunctionKey keysym =
  (keysym >= XK.xk_Select)   && (keysym <= XK.xk_Break)

let isModifierKey keysym =
  ((keysym >= XK.xk_Shift_L) && (keysym <= XK.xk_Hyper_R))
   || (keysym == XK.xk_Mode_switch)
   || (keysym == XK.xk_Num_Lock)

let uTranslateKey display keycode modifiers =
    uKeyInitialize display;
    (shiftMask lor lockMask
      lor  display.dpy_mode_switch lor display.dpy_num_lock,
     if (keycode < display.dpy_min_keycode)
       || (keycode > display.dpy_max_keycode) then
         noSymbol
     else
       let keysym_return =
         let per = ref display.dpy_keysyms_per_keycode in
         let syms = display.dpy_keysyms.(keycode - display.dpy_min_keycode) in
         let offset =
           while (!per > 2) && (syms.(!per - 1) = noSymbol) do
	     (--) per 
           done;
           if (!per > 2) && (modifiers land display.dpy_mode_switch <>0) then
             begin
	       per -= 2;
               2
             end
           else 0
         in
         if ((modifiers land display.dpy_num_lock) <> 0) &&
	   (!per > 1) && ((isKeypadKey syms.(1+offset))
                        || (isPrivateKeypadKey syms.(1+offset)))
         then
	     if (modifiers land shiftMask <> 0) ||
	       ((modifiers land lockMask <> 0) && 
                (display.dpy_lock_meaning = XK.xk_Shift_Lock)) 
             then
	       syms.(offset)
	     else
	       syms.(1+offset)
           else
             if not (modifiers land shiftMask <> 0) &&
	       (not (modifiers land lockMask <> 0) ||
                (display.dpy_lock_meaning = noSymbol)) 
             then
	       if (!per = 1) || (syms.(1+offset) = noSymbol) then
	         fst (convertCase syms.(offset))
	       else
	         syms.(offset)
           else 
             if (not (modifiers land lockMask <> 0) ||
	         (display.dpy_lock_meaning <> XK.xk_Caps_Lock)) then
               begin
	         if (!per = 1) || (syms.(1+offset) = noSymbol) then
	           snd (convertCase syms.(offset))
                 else
                   syms.(1+offset)
               end
             else 
               begin
                 let sym = 
                   if (!per = 1) || (syms.(1+offset) = noSymbol) then
	             syms.(offset)
                   else 
                     syms.(1+offset) in
                 let (lsym,usym) = convertCase sym in 
	           if (not (modifiers land shiftMask <> 0) && 
                       (sym <> syms.(offset)) &&
	               ((sym <> usym) || (lsym = usym))) then
	             snd (convertCase syms.(offset))
                   else
                     usym
               end
       in
         if keysym_return = XK.xk_VoidSymbol then
	   noSymbol else keysym_return)

(* 
val uXKeyInitialize : display -> serverInfo -> unit
val uXTranslateKey : display -> keycode -> state -> modifiers * keysym
val uXTranslateKeySym : display -> keysym -> state -> string
val lookupString : event -> status -> string * keysym * status 
val uXKeyInitialize : display -> serverInfo -> unit
*)


let lookupString display event =
  let kev =
    match event with
        KeyPressEvent t -> t
      | KeyReleaseEvent t -> t
      | _ -> raise (Invalid_argument "lookupString : bad event")
  in
  let (modifiers,keysym) =
    uTranslateKey display kev.Xkey.detail kev.Xkey.state
  in
    (uTranslateKeySym display keysym kev.Xkey.state, keysym, modifiers)
