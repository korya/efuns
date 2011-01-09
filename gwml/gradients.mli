val gradient_levels : int Options.option_record
val hexs : char array
val hex1 : int -> char
val hex0 : int -> char
val gen_file : char -> int -> int -> int -> int -> int -> int -> string
val rgb : string -> int * int * int
val make_vgradient : string -> string -> string
val make_hgradient : string -> string -> string
val make_dgradient : string -> string -> string
val check_filename : string -> unit
