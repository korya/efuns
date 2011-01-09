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

module Ocaml_env = struct
    
    open Env
    open Config
    open Types
    
    type t = {
        values: (Path.t * value_description) Ident.tbl;
        constrs: constructor_description Ident.tbl;
        labels: label_description Ident.tbl;
        types: (Path.t * type_declaration) Ident.tbl;
        modules: (Path.t * module_type) Ident.tbl;
        modtypes: (Path.t * modtype_declaration) Ident.tbl;
        components: (Path.t * module_components) Ident.tbl;
        classes: (Path.t * class_declaration) Ident.tbl;
        cltypes: (Path.t * cltype_declaration) Ident.tbl;
        summary: summary
      }
    
    and module_components =
      Structure_comps of structure_components
    | Functor_comps of functor_components
    
    and structure_components = {
        mutable comp_values: (string, (value_description * int)) Tbl.t;
        mutable comp_constrs: (string, (constructor_description * int)) Tbl.t;
        mutable comp_labels: (string, (label_description * int)) Tbl.t;
        mutable comp_types: (string, (type_declaration * int)) Tbl.t;
        mutable comp_modules: (string, (module_type * int)) Tbl.t;
        mutable comp_modtypes: (string, (modtype_declaration * int)) Tbl.t;
        mutable comp_components: (string, (module_components * int)) Tbl.t;
        mutable comp_classes: (string, (class_declaration * int)) Tbl.t;
        mutable comp_cltypes: (string, (cltype_declaration * int)) Tbl.t
      }
    
    and functor_components = {
        fcomp_param: Ident.t;
        fcomp_arg: module_type;
        fcomp_res: module_type;
        fcomp_env: t
      }
    
    let empty = {
        values = Ident.empty; constrs = Ident.empty;
        labels = Ident.empty; types = Ident.empty;
        modules = Ident.empty; modtypes = Ident.empty;
        components = Ident.empty; classes = Ident.empty;
        cltypes = Ident.empty;
        summary = Env_empty }

(* Persistent structure descriptions *)
    
    type pers_struct =
      { ps_name: string;
        ps_sig: signature;
        ps_comps: module_components;
        ps_crcs: (string * Digest.t) list }
    
    let persistent_structures =
      (Hashtbl.create 17 : (string, pers_struct) Hashtbl.t)
    
    let read_pers_struct modname filename =
      let ic = open_in_bin filename in
      try
        let buffer = String.create (String.length cmi_magic_number) in
        really_input ic buffer 0 (String.length cmi_magic_number);
        if buffer <> cmi_magic_number then begin
            close_in ic;
            raise(Error(Not_an_interface filename))
          end;
        let (name, sign, comps) = input_value ic in
        let crcs = input_value ic in
        close_in ic;
        let ps = { ps_name = name;
            ps_sig = sign;
            ps_comps = comps;
            ps_crcs = crcs } in
        if ps.ps_name <> modname then
          raise(Error(Illegal_renaming(ps.ps_name, filename)));
        Hashtbl.add persistent_structures modname ps;
        ps
      with End_of_file | Failure _ ->
          close_in ic;
          raise(Error(Corrupted_interface(filename)))
    
    let find_pers_struct load_path name =
      try
        Hashtbl.find persistent_structures name
      with Not_found ->
          read_pers_struct name
            (Misc.find_in_path load_path (String.uncapitalize name ^ ".cmi"))
  
  end
  

module Type = struct 
    open Ocaml_env
    open Misc
    open Config
    open Format
    open Typedtree
    open Compile
    open Path
    open Location
    
    let type_buffer name lexbuf path =
      let old_path = !Config.load_path in
      try
        init_path();
        let env = initial_env() in
        let sstr = Parse.implementation lexbuf in
        Config.load_path := path @ !Config.load_path;
        let (str,env) = 
          Typemod.type_implementation name name name env sstr
        in
        Config.load_path := old_path;
        Env.reset_cache (); 
        (str,env)
      with x ->
          Env.reset_cache (); 
          Config.load_path := old_path;
          raise x
    
    type value = Defined | Used | GlobalDefined
    
    let rec iter_structure str list define =
      List.fold_left (fun list item ->
          match item with
            Tstr_eval e -> iter_expression e list 
          | Tstr_value (_,l) ->
              List.fold_left (fun list (pat,e) ->
                  iter_pattern pat (iter_expression e list)  GlobalDefined
              ) list l
          | Tstr_module (_,m) -> iter_module m list
          | Tstr_class l ->
              List.fold_left (fun list (_,_,_,cl) ->
                  iter_class cl list) list l
          | _ -> list
      ) list str
    
    and iter_expression e list =
      match e.exp_desc with
        Texp_ident (p,v) -> (p, e.exp_loc, e.exp_type, Used) :: list
      | Texp_let (_,l,e) ->
          List.fold_left (fun list (pat,e) ->
              iter_pattern pat (iter_expression e list) Defined
          ) (iter_expression e list) l
      | Texp_function l ->
          List.fold_left (fun list (pat,e) ->
              iter_pattern pat (iter_expression e list) Defined
          ) list l
      | Texp_apply (e,l) ->
          List.fold_left (fun list e ->
              iter_expression e list
          ) (iter_expression e list) l
      | Texp_match (e,l) ->
          List.fold_left (fun list (pat,e) ->
              iter_pattern pat (iter_expression e list) Defined
          ) (iter_expression e list) l
      | Texp_try (e,l) ->
          List.fold_left (fun list (pat,e) ->
              iter_pattern pat (iter_expression e list) Defined
          ) (iter_expression e list) l          
      | Texp_tuple l ->
          List.fold_left (fun list e ->
              iter_expression e list
          ) list l
      | Texp_construct (_,l) ->
          List.fold_left (fun list e ->
              iter_expression e list
          ) list l
      | Texp_record (l,None) ->
          List.fold_left (fun list (_,e) ->
              iter_expression e list
          ) list l                    
      | Texp_record (l,Some e) ->
          List.fold_left (fun list (_,e) ->
              iter_expression e list
          ) (iter_expression e list) l                    
      | Texp_field (e,_) ->
          iter_expression e list
      | Texp_setfield (e1,_,e2) -> 
          iter_expression e1 (iter_expression e2 list)
      | Texp_array l ->
          List.fold_left (fun list e ->
              iter_expression e list
          ) list l                              
      | Texp_ifthenelse (e1,e2,None) ->
          iter_expression e1 (iter_expression e2 list)          
      | Texp_ifthenelse (e1,e2,Some e3) ->
          iter_expression e1 (iter_expression e2 (iter_expression e3 list))
      | Texp_sequence (e1,e2) ->
          iter_expression e1 (iter_expression e2 list)                    
      | Texp_while (e1,e2) ->
          iter_expression e1 (iter_expression e2 list)                    
      | Texp_for (id, e1,e2,_,e3) ->
          iter_expression e1 (iter_expression e2 (iter_expression e3 
                ((Pident id, e.exp_loc,e.exp_type, Defined) ::list)))
      | Texp_when (e1,e2) ->
          iter_expression e1 (iter_expression e2 list)
      | Texp_send (e,_) ->
          iter_expression e list
      | Texp_setinstvar (_,_,e) ->
          iter_expression e list
      | Texp_override (_,l) ->
          List.fold_left (fun list (_,e) ->
              iter_expression e list
          ) list l                              
      | Texp_letmodule (_,m,e) ->
          iter_expression e (iter_module m list)                              
      | Texp_instvar _
      | Texp_new _ 
      | Texp_constant _ -> list
    
    
    and iter_module m list =
      match m.mod_desc with
      | Tmod_ident _ -> list
      | Tmod_structure s -> iter_structure s list Defined
      | Tmod_functor (_,_,m) -> iter_module m list
      | Tmod_apply (m1,m2,_) ->  iter_module m1 (iter_module m2 list)
      | Tmod_constraint (m,_,_) -> iter_module m list
    
    and iter_class cl list = list
    
    and iter_pattern pat list define =
      match pat.pat_desc with
        Tpat_any -> list
      | Tpat_var id -> (Pident id, pat.pat_loc,pat.pat_type,define) :: list
      | Tpat_alias (p,id) ->
          (Pident id, pat.pat_loc,pat.pat_type,define) :: 
          (iter_pattern p list define)
      | Tpat_constant _ -> list
      | Tpat_tuple l ->
          List.fold_left (fun list pat ->
              iter_pattern pat list define
          ) list l
      | Tpat_construct (_,l) ->
          List.fold_left (fun list pat ->
              iter_pattern pat list define
          ) list l          
      | Tpat_record l ->
          List.fold_left (fun list (_,pat) ->
              iter_pattern pat list define
          ) list l                    
      | Tpat_array l ->
          List.fold_left (fun list pat ->
              iter_pattern pat list define
          ) list l          
      | Tpat_or (p1,p2) -> iter_pattern p1 (iter_pattern p2 list define) define
      
  end

    
let printtyp_path p = Printtyp.path p
let printtyp_type_expr t = Printtyp.type_expr t
