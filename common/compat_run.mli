module My_Obj :
  sig
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
val not_implemented : string -> 'a -> 'b
val translate : int -> int
val isint : int -> bool
module My_Config :
  sig
    val exec_magic_number : string
    val cmi_magic_number : string
    val cmo_magic_number : string
    val cma_magic_number : string
    val cmx_magic_number : string
    val cmxa_magic_number : string
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end
module My_Ident :
  sig
    val name : Ident.t -> string
  end
module Dumpobj : sig val inputu : in_channel -> int end
module My_Emitcode :
  sig
    val transl_const : Lambda.structured_constant -> My_Obj.t
  end
module Objinfo :
  sig
    val print_digest : string -> unit
    val print_info : Emitcode.compilation_unit -> unit
  end
module Misc : sig val find_in_path : string list -> string -> string end
val input_library_units : in_channel -> Emitcode.compilation_unit list
module Externals : sig val caml_primitives : (string * My_Obj.t) list end
