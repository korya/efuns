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
  Fichier genere (en partie) par Xatom.l (lex) a partir de Xatom.h 

  Utilisation :
     XA_PRIMARY devient XA.xa_primary
  
 *)

open Xtypes

let xa_primary = id_to_atom  1
let xa_secondary = id_to_atom  2
let xa_arc = id_to_atom  3
let xa_atom = id_to_atom  4
let xa_bitmap = id_to_atom  5
let xa_cardinal = id_to_atom  6
let xa_colormap = id_to_atom  7
let xa_cursor = id_to_atom  8
let xa_cut_buffer0 = id_to_atom  9
let xa_cut_buffer1 = id_to_atom  10
let xa_cut_buffer2 = id_to_atom  11
let xa_cut_buffer3 = id_to_atom  12
let xa_cut_buffer4 = id_to_atom  13
let xa_cut_buffer5 = id_to_atom  14
let xa_cut_buffer6 = id_to_atom  15
let xa_cut_buffer7 = id_to_atom  16
let xa_drawable = id_to_atom 17
let xa_font = id_to_atom  18
let xa_integer = id_to_atom  19
let xa_pixmap = id_to_atom  20
let xa_point = id_to_atom  21
let xa_rectangle = id_to_atom  22
let xa_resource_manager = id_to_atom  23
let xa_rgb_color_map = id_to_atom  24
let xa_rgb_best_map = id_to_atom  25
let xa_rgb_blue_map = id_to_atom  26
let xa_rgb_default_map = id_to_atom  27
let xa_rgb_gray_map = id_to_atom  28
let xa_rgb_green_map = id_to_atom  29
let xa_rgb_red_map = id_to_atom  30
let xa_string = id_to_atom  31
let xa_visualid = id_to_atom  32
let xa_window = id_to_atom  33
let xa_wm_command = id_to_atom  34
let xa_wm_hints = id_to_atom  35
let xa_wm_client_machine = id_to_atom  36
let xa_wm_icon_name = id_to_atom  37
let xa_wm_icon_size = id_to_atom  38
let xa_wm_name = id_to_atom  39
let xa_wm_normal_hints = id_to_atom  40
let xa_wm_size_hints = id_to_atom  41
let xa_wm_zoom_hints = id_to_atom  42
let xa_min_space = id_to_atom  43
let xa_norm_space = id_to_atom  44
let xa_max_space = id_to_atom  45
let xa_end_space = id_to_atom  46
let xa_superscript_x = id_to_atom  47
let xa_superscript_y = id_to_atom  48
let xa_subscript_x = id_to_atom  49
let xa_subscript_y = id_to_atom  50
let xa_underline_position = id_to_atom  51
let xa_underline_thickness = id_to_atom  52
let xa_strikeout_ascent = id_to_atom  53
let xa_strikeout_descent = id_to_atom  54
let xa_italic_angle = id_to_atom  55
let xa_x_height = id_to_atom  56
let xa_quad_width = id_to_atom  57
let xa_weight = id_to_atom  58
let xa_point_size = id_to_atom  59
let xa_resolution = id_to_atom  60
let xa_copyright = id_to_atom  61
let xa_notice = id_to_atom  62
let xa_font_name = id_to_atom  63
let xa_family_name = id_to_atom  64
let xa_full_name = id_to_atom  65
let xa_cap_height = id_to_atom  66
let xa_wm_class = id_to_atom  67
let xa_wm_transient_for = id_to_atom  68

let xa_last_predefined = id_to_atom  68
