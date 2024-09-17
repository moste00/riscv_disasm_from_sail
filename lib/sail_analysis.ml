open Common_types
open Sail_utils
open Sail_ast_foreach
open Sail_ast_processor
open Constants

open Libsail
open Ast
open Type_check
open Ast_util

type union_case_type = Named_type of string | Bitvec of int

type sail_types_context = {
  (* Associates each ast case name with the types of its arguments
     Currently assumes a very limited set of possible type arguments:
     justa a flat list of either bitvectors or type ids.
     But Sail union clauses actually allow a much richer language
     e.g. MY_UNION_CASE(bits(32), (bits(16), (bits(8), bits(8))))
     is a valid union clause definition in Sail, but it's too
     complicated to record in this table *)
  union_cases_type_signature : (string, union_case_type list) Hashtbl.t;
  (* Maps every type equivalent to a bitvector into its bitv size *)
  bitv_synonyms : (string, int) Hashtbl.t;
}

type sail_mappings_context = {
  (* Records all tables mapping enums to bitvectors, keyed by mapping name *)
  enum_bitv_mappings_registery : (string, bv2enum_table) Hashtbl.t;
  (* Records all tables mapping structs to bitvector, keyed by mapping name
     Also records the name of the struct type along with the table *)
  struct_bitv_mappings_registery : (string, string * bv2struct_table) Hashtbl.t;
}

type typecheck_env_info = {
  e : Env.t;
  (* All what follows is obtained from the sail typechecker's env, but
     because the getter functions could be hiding expensive computation, we
     obtain them once and store them here *)
  enums : IdSet.t Bindings.t;
}

type sail_analysis_result = {
  type_ctx : sail_types_context;
  mapping_ctx : sail_mappings_context;
  sail_env : typecheck_env_info;
}

let mk_case_arg_from_app id args =
  let constructor_name = id_to_str id in
  if constructor_name <> "bits" then
    failwith ("Unsupported type application " ^ constructor_name)
  else (
    let size = List.nth args 0 in
    Bitvec (sail_bitv_size_to_int size)
  )

let assoc_clause_with_args state _ union_id clause_id typ =
  if ast_sail_def_name = id_to_str union_id then (
    let name = id_to_str clause_id in
    let (Typ_aux (t, _)) = typ in
    match t with
    | Typ_tuple args_typ ->
        let args =
          List.map
            (fun t ->
              match t with
              | Typ_aux (Typ_id id, _) -> Named_type (id_to_str id)
              | Typ_aux (Typ_app (id, args), _) -> mk_case_arg_from_app id args
              | _ ->
                  failwith
                    ("Each ast union clause must have only type IDs as \
                      arguments, found otherwise after the union clause " ^ name
                    )
            )
            args_typ
        in
        Hashtbl.add state.type_ctx.union_cases_type_signature name args
    | Typ_id id ->
        Hashtbl.add state.type_ctx.union_cases_type_signature name
          [Named_type (id_to_str id)]
    | Typ_app (id, args)
      when id_to_str id = "bits" || id_to_str id = "bitvector" ->
        Hashtbl.add state.type_ctx.union_cases_type_signature name
          [Bitvec (sail_bitv_size_to_int (List.nth args 0))]
    | _ -> failwith ("Unsupported type expression after the union case " ^ name)
  )

let collect_bitvec_abbreviations state _ abbrev _ typ =
  let (A_aux (ty, _)) = typ in
  match ty with
  | A_typ (Typ_aux (Typ_app (id, args), _)) when id_to_str id = "bits" ->
      let size = sail_bitv_size_to_int_noexn (List.nth args 0) in
      if size <> -1 then
        Hashtbl.add state.type_ctx.bitv_synonyms (id_to_str abbrev) size
  | _ -> ()

let add_bitv2enum_entry tbl (bitv_str, pat) =
  match pat with
  | MP_id enum_id ->
      let enum_name = id_to_str enum_id in
      Hashtbl.add tbl bitv_str enum_name
  | MP_lit (L_aux (lit, _)) -> (
    let name = match lit with
      | L_true -> "true"
      | L_false -> "false"
      | _ -> failwith "Expression too complex, unsupported"
    in
    Hashtbl.add tbl bitv_str name
  )
  | _ -> failwith "Unsupported pattern in enum <-> bitvec mapping"  

let struct_member_to_kv_pair (id, MP_aux (pat, _)) =
  let error_msg =
    "Unsupported struct member: only bool constants, enum literals, and \
     bitvector constants are supported"
  in

  let key = id_to_str id in
  match pat with
  | MP_lit (L_aux (lit, _) as literal) -> (
      match lit with
      | L_true -> (key, Bool_const true)
      | L_false -> (key, Bool_const false)
      | L_hex _ | L_bin _ -> (key, Bv_const (bitv_literal_to_str literal))
      | _ -> failwith error_msg
    )
  | MP_id enum_lit -> (key, Enum_lit (id_to_str enum_lit))
  | _ -> failwith error_msg

let add_bitv2struct_entry bitv2struct_tbl (bitv_str, pat) =
  match pat with 
  | MP_struct struct_members -> 
      let key_values = List.map struct_member_to_kv_pair struct_members in
      Hashtbl.add bitv2struct_tbl bitv_str key_values
  | _ -> failwith "Only struct <-> bitvec mappings with struct literals are supported"

let mk_bitv2enum mapping_clauses =
  let bitv2enum_tbl = Hashtbl.create (List.length mapping_clauses) in
  let entries = destructure_bitv_mapping mapping_clauses in
  List.iter (add_bitv2enum_entry bitv2enum_tbl) entries;
  bitv2enum_tbl

let mk_bitv2struct mapping_clauses =
  let bitv2struct_tbl = Hashtbl.create (List.length mapping_clauses) in
  let entries = destructure_bitv_mapping mapping_clauses in
  List.iter (add_bitv2struct_entry bitv2struct_tbl) entries;
  bitv2struct_tbl

let is_enum_bitv_mapping enums mapping_type_annotation =
  is_mapping_from_bitv_to_type_id mapping_type_annotation (fun other_type_id ->
      id_to_str other_type_id = "bool"
      || bindings_contains_id enums other_type_id
  )

let is_struct_bitv_mapping env mapping_type_annotation =
  is_mapping_from_bitv_to_type_id mapping_type_annotation (fun other_type_id ->
      Env.is_record other_type_id env
  )

let collect_enum_bitv_mappings state _ id typ_annot clauses =
  match is_enum_bitv_mapping state.sail_env.enums typ_annot with
  | Some (enum_name, bitv_size) ->
      Hashtbl.add state.mapping_ctx.enum_bitv_mappings_registery (id_to_str id)
        (mk_bitv2enum clauses);
      Hashtbl.add state.type_ctx.bitv_synonyms enum_name bitv_size
  | _ -> ()

let collect_struct_bitv_mappings state _ id typ_annot clauses =
  match is_struct_bitv_mapping state.sail_env.e typ_annot with
  | Some (struct_name, bitv_size) ->
      Hashtbl.add state.mapping_ctx.struct_bitv_mappings_registery (id_to_str id)
        (struct_name, mk_bitv2struct clauses);
      Hashtbl.add state.type_ctx.bitv_synonyms struct_name bitv_size
  | _ -> ()

let collect_mappings state __ id typ_annot clauses =
  collect_enum_bitv_mappings state __ id typ_annot clauses;
  collect_struct_bitv_mappings state __ id typ_annot clauses

let analyze ast env =
  let analysis_result =
    {
      type_ctx =
        {
          union_cases_type_signature = Hashtbl.create 50;
          bitv_synonyms = Hashtbl.create 50;
        };
      mapping_ctx =
        {
          enum_bitv_mappings_registery = Hashtbl.create 50;
          struct_bitv_mappings_registery = Hashtbl.create 5;
        };
      sail_env = { e = env; enums = Env.get_enums env };
    }
  in

  let analyzer_processor =
    {
      default_processor with
      process_union_clause = assoc_clause_with_args;
      process_abbrev = collect_bitvec_abbreviations;
      process_mapping = collect_mappings;
    }
  in
  foreach_node ast analyzer_processor analysis_result;
  analysis_result

let get_bv2enum_mapping ana map_name =
  let bv2enum_mappings = ana.mapping_ctx.enum_bitv_mappings_registery in
  try Some (Hashtbl.find bv2enum_mappings map_name) with Not_found -> None

let get_bv2struct_mapping ana map_name =
  let bv2struct_mappings = ana.mapping_ctx.struct_bitv_mappings_registery in
  try Some (Hashtbl.find bv2struct_mappings map_name) with Not_found -> None

let get_size_of_bv_synonym ana name =
  try Some (Hashtbl.find ana.type_ctx.bitv_synonyms name)
  with Not_found -> None

let get_case_arg_size ana case_name arg_idx =
  try
    let typsig =
      Hashtbl.find ana.type_ctx.union_cases_type_signature case_name
    in
    let case_typ = List.nth typsig arg_idx in
    match case_typ with
    | Named_type typ_name ->
        Some (Hashtbl.find ana.type_ctx.bitv_synonyms typ_name)
    | Bitvec size -> Some size
  with Not_found -> None
