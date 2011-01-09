(***********************************************************************)
(*                                                                     *)
(*                             Efuns                                   *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)


module My_Obj = struct  
    type t = A of int | B of float | C of string
    
    external repr : 'a -> t = "%identity"
    external obj : t -> 'a = "%identity"
    external magic : 'a -> 'b = "%identity"
    external is_block : t -> bool = "obj_is_block"
    external tag : t -> int = "obj_tag"
    external size : t -> int = "%obj_size"
    external field : t -> int -> t = "%obj_field"
    external set_field : t -> int -> t -> unit = "%obj_set_field"
    external new_block : int -> int -> t = "obj_block"
    external dup : t -> t = "obj_dup"
    external truncate : t -> int -> unit = "obj_truncate"
  end


open My_Obj

let not_implemented prim_name =
  fun _ ->
    failwith ("Primitive not implemented "^prim_name)
    
module Externals = struct  
external alloc_dummy : int -> 'a = "alloc_dummy"
external update_dummy : 'a -> 'b -> 'c = "update_dummy"
external make_vect: int -> 'a -> 'a array = "make_vect"
external make_vect: int -> 'a -> 'a array = "make_vect"
external md5_chan: in_channel -> int -> t = "md5_chan"
external gc_stat: unit -> t = "gc_stat"
external gc_get: unit -> t = "gc_get"
external gc_set: t -> unit = "gc_set"
external gc_minor: unit -> unit = "gc_minor"
external gc_major: unit -> unit = "gc_major"
external gc_full_major: unit -> unit = "gc_full_major"
external gc_compaction: unit -> unit = "gc_compaction";;
external hash_univ_param: int -> int -> 'a -> int = "hash_univ_param" "noalloc"
external lex_engine: t -> int -> t -> int = "lex_engine"
external input_value: in_channel -> 'a = "input_value"
external obj_is_block: t -> bool = "obj_is_block"
external obj_tag: t -> int = "obj_tag"
external obj_block: int -> int -> t = "obj_block"
external obj_dup: t -> t = "obj_dup"
external obj_truncate: t -> int -> unit = "obj_truncate"
external compare: 'a -> 'a -> int = "compare" "noalloc"
external power_float: float -> float -> float = "power_float" "pow" "float"
external sqrt_float: float -> float = "sqrt_float" "sqrt" "float"
external exp_float: float -> float = "exp_float" "exp" "float"
external log_float: float -> float = "log_float" "log" "float"
external log10_float: float -> float = "log10_float" "log10" "float"
external cos_float: float -> float = "cos_float" "cos" "float"
external sin_float: float -> float = "sin_float" "sin" "float"
external tan_float: float -> float = "tan_float" "tan" "float"
external acos_float: float -> float = "acos_float" "acos" "float"
external asin_float: float -> float = "asin_float" "asin" "float"
external atan_float: float -> float = "atan_float" "atan" "float"
external atan2_float: float -> float -> float = "atan2_float" "atan2" "float"
external cosh_float: float -> float = "cosh_float" "cosh" "float"
external sinh_float: float -> float = "sinh_float" "sinh" "float"
external tanh_float: float -> float = "tanh_float" "tanh" "float"
external ceil_float: float -> float = "ceil_float" "ceil" "float"
external floor_float: float -> float = "floor_float" "floor" "float"
external fmod_float: float -> float -> float = "fmod_float" "fmod" "float"
external frexp_float: float -> float * int = "frexp_float"
external ldexp_float: float -> int -> float = "ldexp_float"
external modf_float: float -> float * float = "modf_float" "modf"
external int_of_string: string -> int = "int_of_string"
external float_of_string: string -> float = "float_of_string"
external create_string: int -> string = "create_string"
external sys_file_exists: string -> bool = "sys_file_exists"
external sys_remove: string -> unit = "sys_remove"
external sys_rename: string -> string -> unit = "sys_rename"
external sys_getenv: string -> string = "sys_getenv"
external sys_system_command: string -> int = "sys_system_command"
external sys_time: unit -> float = "sys_time"
external sys_chdir: string -> unit = "sys_chdir"
external sys_getcwd: unit -> string = "sys_getcwd"
external add_float : float -> float -> float = "add_float"

external setup : out_channel -> t = "terminfo_setup";;
external backup : int -> unit = "terminfo_backup";;
external standout : bool -> unit = "terminfo_standout";;
external resume : int -> unit = "terminfo_resume";;

external array_get_float : t -> float -> t = "array_get_float"
external array_set_float : t -> float -> t -> unit = "array_set_float"
external array_get : t -> int -> t = "array_get"
external array_set : t -> int -> t -> unit = "array_set"
external array_unsafe_get : t -> int -> t = "array_unsafe_get"
external array_unsafe_set : t -> int -> t -> unit = "array_unsafe_set"
external lessthan : t -> t -> bool = "lessthan"
external lessequal : t -> t -> bool = "lessequal"
external string_equal: string -> string -> bool = "string_equal"
external equal: string -> string -> bool = "equal"
external notequal: string -> string -> bool = "notequal"
external bitvect_test: string -> string -> bool = "bitvect_test"
external string_notequal: string -> string -> bool = "string_notequal"
external array_get_addr: 'a array -> int -> 'a = "array_get_addr"
external array_set_addr: 'a array -> int -> 'a -> unit = "array_set_addr"
external blit_string : string -> int -> string -> int -> int -> unit
= "blit_string" "noalloc"
external output_value : t -> 'a -> t -> unit = "output_value"
external output_value_to_string: 'a -> t -> string = "output_value_to_string"
external input_value : t -> 'a = "input_value"
external greaterthan : t -> t -> bool = "greaterthan"
external greaterequal : t -> t -> bool = "greaterequal"

external mul_float: float -> float -> float = "mul_float"
external div_float: float -> float -> float = "div_float"
external eq_float: float -> float -> bool = "eq_float"
external neq_float: float -> float -> bool = "neq_float"
external le_float: float -> float -> bool = "le_float"
external lt_float: float -> float -> bool = "lt_float"
external ge_float: float -> float -> bool = "ge_float"
external gt_float: float -> float -> bool = "gt_float"
external input_value_from_string: string -> int -> 'a = "input_value_from_string"
external marshal_data_size: string -> int -> int = "marshal_data_size"
external format_int: string -> int -> string = "format_int"
external caml_open_descriptor: int -> out_channel = "caml_open_descriptor"
external channel_descriptor: in_channel -> Unix.file_descr = "channel_descriptor"
external caml_close_channel: out_channel -> unit = "caml_close_channel"
external caml_channel_size:  out_channel -> int = "caml_channel_size"
external caml_set_binary_mode: out_channel -> bool -> unit = "caml_set_binary_mode"
external caml_flush_partial: out_channel -> bool = "caml_flush_partial"
external caml_flush: out_channel -> unit = "caml_flush"
external caml_output_char: out_channel -> char -> unit = "caml_output_char"
external caml_output_int: out_channel -> int -> unit = "caml_output_int"
external caml_output_partial: out_channel -> string -> int -> int -> int = "caml_output_partial"
external caml_output:  out_channel -> string -> int -> int -> unit = "caml_output"
external caml_seek_out: out_channel -> int -> unit = "caml_seek_out"
external caml_pos_out: out_channel -> int = "caml_pos_out"
external caml_input_char: in_channel -> char = "caml_input_char"
external caml_input_int: in_channel -> int = "caml_input_int"
external caml_input: in_channel -> string -> int -> int -> int = "caml_input"
external caml_seek_in: in_channel -> int -> unit = "caml_seek_in"
external caml_pos_in: in_channel -> int = "caml_pos_in"
external caml_input_scan_line: in_channel -> int = "caml_input_scan_line"
external md5_string: string -> int -> int -> t = "md5_string"
let get_current_environment () = not_implemented "get_current_environment"
external parse_engine: Parsing.parse_tables -> Parsing.parser_env -> 'a -> My_Obj.t -> 'b = "parse_engine"
external install_signal_handler: int -> Sys.signal_behavior -> Sys.signal_behavior = "install_signal_handler"
external fill_string: string -> int -> int -> char -> unit = "fill_string"
external is_printable: char -> bool = "is_printable"
external sys_exit: int -> 'a = "sys_exit"
external sys_open: string -> open_flag list -> int -> int = "sys_open"
external sys_close: int -> unit = "sys_close"
external sys_get_argv: unit -> string array = "sys_get_argv"
external sys_get_config: unit -> string * int = "sys_get_config"
external register_named_value: string -> My_Obj.t -> unit = "register_named_value"
external weak_create: int -> 'a Weak.t = "weak_create"
external weak_set: 'a Weak.t -> int -> 'a option -> unit = "weak_set";;
external weak_get: 'a Weak.t -> int -> 'a option = "weak_get"
external weak_check: 'a Weak.t -> int -> bool = "weak_check"
external array_unsafe_get_float: float array -> int -> float = "array_unsafe_get_float"
external array_unsafe_set_addr: 'a array -> int -> 'a -> unit= "array_unsafe_set_addr"
external array_unsafe_set_float: float array -> int -> float -> unit= "array_unsafe_set_float"
external make_array: 'a array -> 'a array = "make_array"
external output_value_to_buffer: string -> int -> int -> 'a -> int list -> int = "output_value_to_buffer"
external format_float:  string -> float -> string = "format_float"
external int_of_float: float -> int = "int_of_float"
external float_of_int: int -> float = "float_of_int"
external neg_float: float -> float = "neg_float"
external abs_float: float -> float = "abs_float"
external sub_float: float -> float -> float = "sub_float"
external gc_counters : unit -> (int * int * int) = "gc_counters"
external sys_random_seed: unit -> int = "sys_random_seed"
external finalise : ('a -> unit) -> 'a -> unit = "final_register"




external int32_neg : int32 -> int32 = "%int32_neg"
external int32_add : int32 -> int32 -> int32 = "%int32_add"
external int32_sub : int32 -> int32 -> int32 = "%int32_sub"
external int32_mul : int32 -> int32 -> int32 = "%int32_mul"
external int32_div : int32 -> int32 -> int32 = "%int32_div"
external int32_mod : int32 -> int32 -> int32 = "%int32_mod"
external int32_and : int32 -> int32 -> int32 = "%int32_and"
external int32_or : int32 -> int32 -> int32 = "%int32_or"
external int32_xor : int32 -> int32 -> int32 = "%int32_xor"
external int32_lsl : int32 -> int -> int32 = "%int32_lsl"
external int32_asr : int32 -> int -> int32 = "%int32_asr"
external int32_lsr : int32 -> int -> int32 = "%int32_lsr"
external int32_shift_right_unsigned: int32 -> int -> int32 = "int32_shift_right_unsigned"
external int32_of_int : int -> int32 = "%int32_of_int"
external int32_to_int : int32 -> int = "%int32_to_int"
external int32_format : string -> int32 -> string = "int32_format"
external int32_of_string : string -> int32 = "int32_of_string"
external int32_to_float : int32 -> float = "int32_to_float"
external int32_of_float : float -> int32 = "int32_of_float"

external int64_neg : int64 -> int64 = "%int64_neg"
external int64_add : int64 -> int64 -> int64 = "%int64_add"
external int64_sub : int64 -> int64 -> int64 = "%int64_sub"
external int64_mul : int64 -> int64 -> int64 = "%int64_mul"
external int64_div : int64 -> int64 -> int64 = "%int64_div"
external int64_mod : int64 -> int64 -> int64 = "%int64_mod"
external int64_and : int64 -> int64 -> int64 = "%int64_and"
external int64_or : int64 -> int64 -> int64 = "%int64_or"
external int64_xor : int64 -> int64 -> int64 = "%int64_xor"
external int64_lsl : int64 -> int -> int64 = "%int64_lsl"
external int64_asr : int64 -> int -> int64 = "%int64_asr"
external int64_lsr : int64 -> int -> int64 = "%int64_lsr"
external int64_of_int : int -> int64 = "%int64_of_int"
external int64_to_int : int64 -> int = "%int64_to_int"
external int64_of_int32 : int32 -> int64 = "%int64_of_int32"
external int64_to_int32 : int64 -> int32 = "%int64_to_int32"
external int64_of_nativeint : nativeint -> int64 = "%int64_of_nativeint"
external int64_to_nativeint : int64 -> nativeint = "%int64_to_nativeint"
external int64_format : string -> int64 -> string = "int64_format"
external int64_of_string : string -> int64 = "int64_of_string"
external int64_shift_right_unsigned: int32 -> int -> int32 = "int64_shift_right_unsigned"
external int64_to_float : int64 -> float = "int64_to_float"
external int64_of_float : float -> int64 = "int64_of_float"
external nativeint_neg: nativeint -> nativeint = "%nativeint_neg"
external nativeint_add: nativeint -> nativeint -> nativeint = "%nativeint_add"
external nativeint_sub: nativeint -> nativeint -> nativeint = "%nativeint_sub"
external nativeint_mul: nativeint -> nativeint -> nativeint = "%nativeint_mul"
external nativeint_div: nativeint -> nativeint -> nativeint = "%nativeint_div"
external nativeint_mod: nativeint -> nativeint -> nativeint = "%nativeint_mod"
external nativeint_and: nativeint -> nativeint -> nativeint = "%nativeint_and"
external nativeint_or: nativeint -> nativeint -> nativeint = "%nativeint_or"
external nativeint_xor: nativeint -> nativeint -> nativeint = "%nativeint_xor"
external nativeint_shift_left: nativeint -> int -> nativeint = "%nativeint_lsl"
external nativeint_shift_right: nativeint -> int -> nativeint = "%nativeint_asr"
external nativeint_shift_right_logical: nativeint -> int -> nativeint = "%nativeint_lsr"
external nativeint_shift_right_unsigned: nativeint -> int -> nativeint = "nativeint_shift_right_unsigned"
external nativeint_of_int: int -> nativeint = "%nativeint_of_int"
external nativeint_to_int: nativeint -> int = "%nativeint_to_int"
external nativeint_of_int32: int32 -> nativeint = "%nativeint_of_int32"
external nativeint_to_int32: nativeint -> int32 = "%nativeint_to_int32"
external nativeint_format : string -> nativeint -> string = "nativeint_format"
external nativeint_of_string: string -> nativeint = "nativeint_of_string"
external nativeint_of_float: float -> nativeint = "nativeint_of_float"
external nativeint_to_float: nativeint -> float = "nativeint_to_float"

let caml_primitives =
  [
    "ml_string_length", repr String.length;
    "string_set", repr String.set;
    "string_get", repr String.get;
    "make_vect", repr make_vect;
    "make_vect", repr make_vect;
    "md5_chan", repr md5_chan;
    "gc_stat", repr gc_stat;
    "gc_get", repr gc_get;
    "gc_set", repr gc_set;
    "gc_minor", repr gc_minor;
    "gc_major", repr gc_major;
    "gc_full_major", repr gc_full_major;
    "gc_compaction", repr gc_compaction;
    "hash_univ_param", repr hash_univ_param;
    "lex_engine", repr lex_engine;
    "input_value", repr input_value;
    "obj_is_block", repr obj_is_block;
    "obj_tag", repr obj_tag;
    "obj_block", repr obj_block;
    "obj_dup", repr obj_dup;
    "obj_truncate", repr obj_truncate;
    "compare", repr compare;
    "power_float", repr power_float;
    "sqrt_float", repr sqrt_float;
    "exp_float", repr exp_float;
    "log_float", repr log_float;
    "log10_float", repr log10_float;
    "cos_float", repr cos_float;
    "sin_float", repr sin_float;
    "tan_float", repr tan_float;
    "acos_float", repr acos_float;
    "asin_float", repr asin_float;
    "atan_float", repr atan_float;
    "atan2_float", repr atan2_float;
    "cosh_float", repr cosh_float;
    "sinh_float", repr sinh_float;
    "tanh_float", repr tanh_float;
    "ceil_float", repr ceil_float;
    "floor_float", repr floor_float;
    "fmod_float", repr fmod_float;
    "frexp_float", repr frexp_float;
    "ldexp_float", repr ldexp_float;
    "modf_float", repr modf_float;
    "int_of_string", repr int_of_string;
    "float_of_string", repr float_of_string;
    "create_string", repr create_string;
    "sys_file_exists", repr sys_file_exists;
    "sys_remove", repr sys_remove;
    "sys_rename", repr sys_rename;
    "sys_getenv", repr sys_getenv;
    "sys_system_command", repr sys_system_command;
    "sys_time", repr sys_time;
    "sys_chdir", repr sys_chdir;
    "sys_getcwd", repr sys_getcwd;
    "string_equal", repr string_equal;
    "equal", repr equal;
    "notequal", repr notequal;
    "lessthan", repr lessthan;
    "lessequal", repr lessequal;
    "array_set", repr array_set;
    "array_get", repr array_get;
    "array_set_float", repr array_set_float;
    "array_get_float", repr array_get_float;
    "add_float", repr add_float;
    "array_unsafe_set", repr array_unsafe_set;
    "array_unsafe_get", repr array_unsafe_get;
    "blit_string", repr blit_string;
    
    "bitvect_test", repr bitvect_test;
    "string_notequal", repr string_notequal;
    "array_get_addr", repr array_get_addr;
    "array_set_addr", repr array_set_addr;
    "terminfo_setup", repr setup ;
    "terminfo_backup", repr backup ;
    "terminfo_standout", repr standout ;
    "terminfo_resume", repr resume ;
    
    "get_global_data", repr (not_implemented "get_global_data");
    "realloc_global", repr (not_implemented "realloc_global", repr );
    "static_alloc", repr (not_implemented "static_alloc");
    "static_free", repr (not_implemented "static_free");
    "static_resize", repr (not_implemented "static_resize", repr );
    "reify_bytecode", repr (not_implemented "reify_bytecode");
    "available_primitives", repr (not_implemented "available_primitives", repr );
    "invoke_traced_function", repr (not_implemented "invoke_traced_function");
    
    
    "output_value_to_string", repr output_value_to_string;
    "output_value", repr output_value;
    "input_value", repr input_value;
    
    "alloc_dummy", repr alloc_dummy;
    "update_dummy", repr update_dummy;
    "greaterthan", repr greaterthan;
    "greaterequal", repr greaterequal;
    
    "mul_float", repr mul_float;
    "div_float", repr div_float;
    "eq_float", repr eq_float;
    "neq_float", repr neq_float;
    "le_float", repr le_float;
    "lt_float", repr lt_float;
    "ge_float", repr ge_float;
    "gt_float", repr gt_float;
    "input_value_from_string", repr input_value_from_string;
    "marshal_data_size", repr marshal_data_size;
    "format_int", repr format_int;
    "caml_open_descriptor", repr caml_open_descriptor;
    "channel_descriptor", repr channel_descriptor;
    "caml_close_channel", repr caml_close_channel;
    "caml_channel_size", repr caml_channel_size;
    "caml_set_binary_mode", repr caml_set_binary_mode;
    "caml_flush_partial", repr caml_flush_partial;
    "caml_flush", repr caml_flush;
    "caml_output_char", repr caml_output_char;
    "caml_output_int", repr caml_output_int;
    "caml_output_partial", repr caml_output_partial;
    "caml_output", repr caml_output;
    "caml_seek_out", repr caml_seek_out;
    "caml_pos_out", repr caml_pos_out;
    "caml_input_char", repr caml_input_char;
    "caml_input_int", repr caml_input_int;
    "caml_input", repr caml_input;
    "caml_seek_in", repr caml_seek_in;
    "caml_pos_in", repr caml_pos_in;
    "caml_input_scan_line", repr caml_input_scan_line;
    "md5_string", repr md5_string;
    "get_current_environment", repr get_current_environment;
    "parse_engine", repr parse_engine;
    "install_signal_handler", repr install_signal_handler;
    "fill_string", repr fill_string;
    "is_printable", repr is_printable;
    "sys_exit", repr sys_exit;
    "sys_open", repr sys_open;
    "sys_close", repr sys_close;
    "sys_get_argv", repr sys_get_argv;
    "sys_get_config", repr sys_get_config;
    "register_named_value", repr register_named_value;
    "weak_create", repr weak_create;
    "weak_set", repr weak_set;
    "weak_get", repr weak_get;
    "weak_check", repr weak_check;
    "array_unsafe_get_float", repr array_unsafe_get_float;
    "array_unsafe_set_addr", repr array_unsafe_set_addr;
    "array_unsafe_set_float", repr array_unsafe_set_float;
    "make_array", repr make_array;
    "output_value_to_buffer", repr output_value_to_buffer;
    "format_float", repr format_float;
    "int_of_float", repr int_of_float;
    "float_of_int", repr float_of_int;
    "neg_float", repr neg_float;
    "abs_float", repr abs_float;
    "sub_float", repr sub_float;
    "gc_counters", repr gc_counters;
    "sys_random_seed", repr sys_random_seed;
    "final_register", repr finalise;


  "nativeint_neg", repr nativeint_neg;
  "nativeint_add", repr nativeint_add;
  "nativeint_sub", repr nativeint_sub;
  "nativeint_mul", repr nativeint_mul;
  "nativeint_div", repr nativeint_div;
  "nativeint_mod", repr nativeint_mod;
  "nativeint_and", repr nativeint_and;
  "nativeint_or", repr nativeint_or;
  "nativeint_xor", repr nativeint_xor;
  "nativeint_shift_left", repr nativeint_shift_left;
  "nativeint_shift_right", repr nativeint_shift_right;
  "nativeint_shift_right_logical", repr nativeint_shift_right_logical;
  "nativeint_shift_right_unsigned", repr nativeint_shift_right_unsigned;
  "nativeint_of_int", repr nativeint_of_int;
  "nativeint_to_int", repr nativeint_to_int;
  "nativeint_of_int32", repr nativeint_of_int32;
  "nativeint_to_int32", repr nativeint_to_int32;
  "nativeint_format", repr nativeint_format;
  "nativeint_of_string", repr nativeint_of_string;

"int32_neg", repr int32_neg;
"int32_add", repr int32_add;
"int32_sub", repr int32_sub;
"int32_mul", repr int32_mul;
"int32_div", repr int32_div;
"int32_mod", repr int32_mod;
"int32_and", repr int32_and;
"int32_or", repr int32_or;
"int32_xor", repr int32_xor;
"int32_lsl", repr int32_lsl;
"int32_shift_left", repr int32_lsl;
"int32_asr", repr int32_asr;
"int32_shift_right", repr int32_asr;
"int32_lsr", repr int32_lsr;
"int32_shift_right_unsigned", repr int32_shift_right_unsigned;
"int32_of_int", repr int32_of_int;
"int32_to_int", repr int32_to_int;
"int32_format", repr int32_format;
"int32_of_string", repr int32_of_string;
"int64_neg", repr int64_neg;
"int64_add", repr int64_add;
"int64_sub", repr int64_sub;
"int64_mul", repr int64_mul;
"int64_div", repr int64_div;
"int64_mod", repr int64_mod;
"int64_and", repr int64_and;
"int64_or", repr int64_or;
"int64_xor", repr int64_xor;
"int64_lsl", repr int64_lsl;
"int64_shift_left", repr int64_lsl;
"int64_asr", repr int64_asr;
"int64_shift_right", repr int64_asr;
"int64_lsr", repr int64_lsr;
"int64_shift_right_unsigned", repr int64_shift_right_unsigned;
"int64_of_int", repr int64_of_int;
"int64_to_int", repr int64_to_int;
"int64_of_int32", repr int64_of_int32;
"int64_to_int32", repr int64_to_int32;
"int64_of_nativeint", repr int64_of_nativeint;
"int64_to_nativeint", repr int64_to_nativeint;
"int64_format", repr int64_format;
    "int64_of_string", repr int64_of_string;
    "int32_to_float", repr int32_to_float;
    "int32_of_float", repr int32_of_float;
    "int64_of_float", repr int64_of_float;
    "int64_to_float", repr int64_to_float;
    "nativeint_of_float", repr nativeint_of_float;
    "nativeint_to_float", repr nativeint_to_float;
]
end

let translate op = op
external isint : 'a -> bool = "%obj_is_int"


    
(* from typing/ident.ml *)
module My_Ident = struct
    type t = { stamp: int; name: string; mutable global: bool }
    let name i = (Obj.magic i).name
  end

(* from parsing/asttypes.mli 
module Asttypes = struct
    type constant =
      Const_int of int
    | Const_char of char
    | Const_string of string
    | Const_float of string
  end

(* from bytecomp/lambda.ml *)
module Lambda = struct
    open Asttypes
    
    type structured_constant =
      Const_base of constant
    | Const_pointer of int
    | Const_block of int * structured_constant list
    | Const_float_array of string list
  end
*)

module Dumpobj = struct
    let inputu ic =
      let b1 = input_byte ic in
      let b2 = input_byte ic in
      let b3 = input_byte ic in
      let b4 = input_byte ic in
      (b4 lsl 24) + (b3 lsl 16) + (b2 lsl 8) + b1
  end

(* Translate structured constants *)

module My_Emitcode = struct 
(* from bytecomp/emitcode.ml *)
    open Lambda
    open Asttypes
    open Emitcode
      (*
    type reloc_info =
      Reloc_literal of structured_constant    (* structured constant *)
    | Reloc_getglobal of Ident.t             (* reference to a global *)
    | Reloc_setglobal of Ident.t             (* definition of a global *)
| Reloc_primitive of string               (* C primitive number *)
  
    type compilation_unit =
      { cu_name: string;                    (* Name of compilation unit *)
        mutable cu_pos: int;                (* Absolute position in file *)
        cu_codesize: int;                   (* Size of code block *)
        cu_reloc: (reloc_info * int) list;  (* Relocation information *)
        cu_imports: (string * Digest.t) list;
                 (* Names and CRC of intfs imported *)
        cu_primitives: string list;    (* Primitives declared inside *)
        mutable cu_force_link: bool;   (* Must be linked even if unref'ed *)
        mutable cu_debug: int;         (* Position of debugging info, or 0 *)
        cu_debugsize: int }            (* Length of debugging info *)
*)    
    
    let rec transl_const = function
        Const_base(Const_int i) -> My_Obj.repr i
      | Const_base(Const_char c) -> My_Obj.repr c
      | Const_base(Const_string s) -> My_Obj.repr s
      | Const_base(Const_float f) -> My_Obj.repr(float_of_string f)
      | Const_pointer i -> My_Obj.repr i
      | Const_block(tag, fields) ->
          let block = My_Obj.new_block tag (List.length fields) in
          let pos = ref 0 in
          List.iter
            (fun c -> My_Obj.set_field block !pos (transl_const c); incr pos)
          fields;
          block
      | Const_float_array fields ->
          My_Obj.repr(Array.of_list(List.map (fun f -> float_of_string f) fields))
          
          (*
          type library =
  { lib_units: compilation_unit list;   (* List of compilation units *)
    lib_custom: bool;                   (* Requires custom mode linking? *)
    lib_ccobjs: string list;            (* C object files needed *)
    lib_ccopts: string list }           (* Extra opts to C compiler *)
*)
  end

(* from tools/objinfo.ml *)

module Objinfo = struct
    open Emitcode
    
    let print_digest d =
      for i = 0 to String.length d - 1 do
        Printf.printf "%02x" (Char.code d.[i])
      done
    
    let print_info cu =
      print_string "  Unit name: "; print_string cu.cu_name; print_newline();
      print_string "  Interfaces imported:"; print_newline();
      List.iter
        (fun (name, digest) ->
          print_string "\t"; print_digest digest; print_string "\t";
          print_string name; print_newline())
      cu.cu_imports;
      print_string "  Uses unsafe features: ";
      begin match cu.cu_primitives with
          [] -> print_string "no"; print_newline()
        | l  -> print_string "YES"; print_newline();
            print_string "  Primitives declared in this module:";
            print_newline();
            List.iter
              (fun name -> print_string "\t"; print_string name; print_newline())
            l
      end
  end

(* from utils/misc.ml *)
module Misc = struct
    let find_in_path path name =
      if not (Filename.is_implicit name) then
        if Sys.file_exists name then name else raise Not_found
      else begin
          let rec try_dir = function
              [] -> raise Not_found
            | dir::rem ->
                let fullname = Filename.concat dir name in
                if Sys.file_exists fullname then fullname else try_dir rem
          in try_dir path
        end
        
        
  end

open Emitcode
  
let input_library_units ic =
  let toc = (input_value ic : Emitcode.library) in
  toc.lib_units
          

(* from utils/config.ml *)
module My_Config = struct
