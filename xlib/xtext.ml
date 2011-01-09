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


open Xtypes
            
exception DefaultChar;;
    
let ci_nonexistchar cs =
  (cs.char_width 
     lor cs.char_rbearing
     lor cs.char_lbearing
     lor cs.char_ascent
     lor cs.char_descent) = 0
    ;;

(* 
   CI_GET_CHAR_INFO_1D - return the charinfo struct for the indicated 8bit
   character.  If the character is in the column and exists, then return the
   appropriate metrics (note that fonts with common per-character metrics
   will return min_bounds).  If none of these hold true, try again with the
   default char.
   fs : fontInfo
   per_char : charInfo array
   col : colonne
   def : charInfo
   *)
let ci_get_char_info_1d fs per_char col =
  if (col >= fs.font_min_char_or_byte2 &
      col <= fs.font_max_char_or_byte2) then
    if (per_char = [||]) then
      fs.font_min_bounds
    else
      let cs = per_char.((col - fs.font_min_char_or_byte2))
      in
      if (ci_nonexistchar cs) then 
        raise DefaultChar
      else
        cs
  else
    raise DefaultChar
      ;;
    
let ci_get_default_info_1d fs per_char =
  ci_get_char_info_1d fs per_char fs.font_default_char
    ;;

(*
  CI_GET_CHAR_INFO_2D - return the charinfo struct for the indicated row
  and column.  This is used for fonts that have more than row zero.
  *)
let ci_get_char_info_2d fs per_char row col =
  if (row >= fs.font_min_byte1 & row <= fs.font_max_byte1 
        && col >= fs.font_min_char_or_byte2 
        && col <= fs.font_max_char_or_byte2) then
    if (per_char = [||]) then
      fs.font_min_bounds
    else 
      let cs = per_char.(((row - fs.font_min_byte1) * 
                            (fs.font_max_char_or_byte2 - 
                               fs.font_min_char_or_byte2 + 1)) + 
                           (col - fs.font_min_char_or_byte2))
      in
      if (ci_nonexistchar cs) then
        raise DefaultChar
      else
        cs
  else
    raise DefaultChar
      ;;   
    
let ci_get_default_info_2d fs per_char =
  let row = fs.font_default_char lsr 8
  and col = fs.font_default_char land 255
  in
  ci_get_char_info_2d fs per_char row col
    ;;

(* 
   CI_GET_ROWZERO_CHAR_INFO_2D - do the same thing as CI_GET_CHAR_INFO_1D,
   except that the font has more than one row.  This is special case of more
   general version used in XTextExt16.c since row == 0.  This is used when
   max_char_or_byte2 is not zero.  A further optimization would do the check
   for min_byte1 being zero ahead of time.
   *)

let ci_get_rawzero_char_info_2d fs per_char col =
  if (fs.font_min_byte1 == 0
	&& col >= fs.font_min_char_or_byte2 
        && col <= fs.font_max_char_or_byte2) then
    if (per_char = [||]) then
      fs.font_min_bounds
    else
      let cs = per_char.((col - fs.font_min_char_or_byte2))
      in
      if (ci_nonexistchar cs) then
        raise DefaultChar
      else 
        cs
  else
    raise DefaultChar
      ;;


(*
  XTextExtents - compute the extents of string given as a sequences of eight
  bit bytes.  Since we know that the input characters will always be from
  the first row of the font (i.e. byte1 == 0), we can do some optimizations
  beyond what is done in XTextExtents16.
  *)

let extents fontstr string =
  let (fs,per_char) = (fontstr.qf_info,fontstr.qf_chars)
  in
  let nchars = String.length string
  and singlerow = (fs.font_max_byte1 = 0)
  in
  let tdef = 
    try
      Some (
      if singlerow then
        ci_get_default_info_1d fs per_char
      else
        ci_get_default_info_2d fs per_char
          )
    with
      DefaultChar -> None
  and (overall : charInfo option ref) = ref None
  in
  (*
    Iterate over the input string getting the appropriate * char struct.
    The default (which may be null if there is no def_char) will be returned
    if the character doesn't exist.  On the first time * through the loop,
    assign the values to overall; otherwise, compute * the new values.
    *)
  for i=0 to nchars-1 do
    let uc = string.[i]
    in
    match (
      (try 
        Some (  
        if singlerow then
          ci_get_char_info_1d fs per_char (Char.code uc)
        else
          ci_get_rawzero_char_info_2d fs per_char (Char.code uc)
            )
      with
        DefaultChar -> tdef
            ),
      !overall
        ) with
      (None,_) -> ()
    | (Some cs,None) -> overall := Some {
         char_lbearing = cs.char_lbearing;
         char_rbearing = cs.char_rbearing;
         char_width = cs.char_width;
         char_ascent = cs.char_ascent;
         char_descent = cs.char_descent;
         char_attributes = cs.char_attributes
       }
    | (Some cs,Some overall) ->
        overall.char_ascent <- max overall.char_ascent cs.char_ascent;
        overall.char_descent <- max overall.char_descent cs.char_descent;
        overall.char_lbearing <- min overall.char_lbearing 
             (overall.char_width + cs.char_lbearing);
        overall.char_rbearing <- max overall.char_rbearing
             (overall.char_width + cs.char_rbearing);
        overall.char_width <- overall.char_width + cs.char_width
  done;
  
  match !overall with
    None ->  {  char_lbearing = 0;
               char_rbearing = 0;
               char_width = 0;
               char_ascent = 0;
               char_descent = 0;
               char_attributes = 0 }
  | Some overall -> overall
        ;;


(*
  * XTextWidth - compute the width of a string of eightbit bytes.  This is a 
  * subset of XTextExtents.
  *)
let width fontstr string =
  (extents fontstr string).char_width
    ;;

