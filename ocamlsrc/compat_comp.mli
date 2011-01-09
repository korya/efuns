module Ocaml_env :
  sig
    type t =
      { values: (Path.t * Types.value_description) Ident.tbl;
        constrs: Types.constructor_description Ident.tbl;
        labels: Types.label_description Ident.tbl;
        types: (Path.t * Types.type_declaration) Ident.tbl;
        modules: (Path.t * Types.module_type) Ident.tbl;
        modtypes: (Path.t * Types.modtype_declaration) Ident.tbl;
        components: (Path.t * module_components) Ident.tbl;
        classes: (Path.t * Types.class_declaration) Ident.tbl;
        cltypes: (Path.t * Types.cltype_declaration) Ident.tbl;
        summary: Env.summary }
    and module_components =
      | Structure_comps of structure_components
      | Functor_comps of functor_components
    and structure_components =
      { mutable comp_values: (string, Types.value_description * int) Tbl.t;
        mutable comp_constrs: (string, Types.constructor_description * int)
                              Tbl.t;
        mutable comp_labels: (string, Types.label_description * int) Tbl.t;
        mutable comp_types: (string, Types.type_declaration * int) Tbl.t;
        mutable comp_modules: (string, Types.module_type * int) Tbl.t;
        mutable comp_modtypes: (string, Types.modtype_declaration * int)
                               Tbl.t;
        mutable comp_components: (string, module_components * int) Tbl.t;
        mutable comp_classes: (string, Types.class_declaration * int) Tbl.t;
        mutable comp_cltypes: (string, Types.cltype_declaration * int) Tbl.t }
    and functor_components =
      { fcomp_param: Ident.t;
        fcomp_arg: Types.module_type;
        fcomp_res: Types.module_type;
        fcomp_env: t }
    val empty : t
    type pers_struct =
      { ps_name: string;
        ps_sig: Types.signature;
        ps_comps: module_components;
        ps_crcs: (string * Digest.t) list }
    val persistent_structures : (string, pers_struct) Hashtbl.t
    val read_pers_struct : string -> string -> pers_struct
    val find_pers_struct : string list -> string -> pers_struct
  end
module Type :
  sig
    val type_buffer :
      string ->
      Lexing.lexbuf ->
      string list -> Typedtree.structure * Typedtree.module_coercion
    type value = | Defined | Used | GlobalDefined
    val iter_structure :
      Typedtree.structure ->
      (Path.t * Location.t * Types.type_expr * value) list ->
      value -> (Path.t * Location.t * Types.type_expr * value) list
    val iter_expression :
      Typedtree.expression ->
      (Path.t * Location.t * Types.type_expr * value) list ->
      (Path.t * Location.t * Types.type_expr * value) list
    val iter_module :
      Typedtree.module_expr ->
      (Path.t * Location.t * Types.type_expr * value) list ->
      (Path.t * Location.t * Types.type_expr * value) list
    val iter_class :
      Typedtree.class_expr ->
      (Path.t * Location.t * Types.type_expr * value) list ->
      (Path.t * Location.t * Types.type_expr * value) list
    val iter_pattern :
      Typedtree.pattern ->
      (Path.t * Location.t * Types.type_expr * value) list ->
      value -> (Path.t * Location.t * Types.type_expr * value) list
  end
val printtyp_path : Path.t -> unit
val printtyp_type_expr : Types.type_expr -> unit
val printlambda_lambda : Format.formatter -> Lambda.lambda -> unit
val printinstr_instrlist : 
  Format.formatter -> Instruct.instruction list -> unit
val printtyp_ident : Format.formatter -> Ident.t -> unit
val primitive_print_description :  
  Format.formatter -> Primitive.description -> unit
val printtyp_type_scheme:  
  Format.formatter -> Types.type_expr -> unit
val print_value: Env.t -> Obj.t -> Format.formatter -> Types.type_expr -> unit
val printtyp_type_declaration:  
  Ident.t -> Format.formatter -> Types.type_declaration -> unit
val printtyp_exception_declaration:  
  Ident.t -> Format.formatter -> Types.exception_declaration -> unit
  
val printtyp_modtype:
  Format.formatter -> Types.module_type -> unit
val printtyp_modtype_declaration :
  Ident.t -> Format.formatter -> Types.modtype_declaration -> unit
val printtyp_class_declaration : 
  Ident.t -> Format.formatter -> Types.class_declaration -> unit
val printtyp_cltype_declaration : 
  Ident.t -> Format.formatter -> Types.cltype_declaration -> unit
 
  
val errors_report_error : Format.formatter -> exn -> unit
val compile_implementation : Format.formatter -> string -> unit