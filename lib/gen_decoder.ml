open Sail_ast_foreach
open Sail_ast_processor
open Sail_ast_utils
open Constants
open Decoder
open Decode_procedure

open Libsail
open Ast

type union_case_arg = Named_type of string | Bitvec of int64

type bitv2enum = (int64, string) Hashtbl.t

type 'a set = ('a, unit) Hashtbl.t

let set_add set thing = Hashtbl.add set thing ()

let set_contains set thing =
  try
    Hashtbl.find set thing;
    true
  with Not_found -> false

type decoder_gen_iteration_state = {
  (* Associates each ast case name with the types of its arguments
     Currently assumes a very limited set of possible type arguments
     although Sail union clauses actually allow a much richer language
     e.g. MY_UNION_CASE(bits(32), (bits(16), (bits(8), bits(8))))
     is a valid union clause definition in Sail, but it's too
     complicated to record in this table *)
  union_case_to_args : (string, union_case_arg list) Hashtbl.t;
  (* Records the names of all enum types *)
  enum_names : string set;
  (* Maps every type equivalent to a bitvector into its bitv size *)
  bitv_synonyms : (string, int64) Hashtbl.t;
  (* Records all tables mapping enums to bitvectors *)
  enum_bitv_mappings_registery : (string, bitv2enum) Hashtbl.t;
  mutable decode_rules : decoder;
  (* The length of the bitstream input to the decoder *)
  mutable instr_length : int64 option;
}

let mk_case_arg_from_app id args =
  let constructor_name = id_to_str id in
  if constructor_name <> "bits" then
    failwith ("Unsupported type application " ^ constructor_name)
  else (
    let size = List.nth args 0 in
    Bitvec (sail_bitv_size_to_int64 size)
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
        Hashtbl.add state.union_case_to_args name args
    | Typ_id id ->
        Hashtbl.add state.union_case_to_args name [Named_type (id_to_str id)]
    | _ -> failwith ("Unsupported type expression after the union case " ^ name)
  )

let collect_enums state _ id _ _ = set_add state.enum_names (id_to_str id)

let collect_bitvec_abbreviations state _ abbrev _ typ =
  let (A_aux (ty, _)) = typ in
  match ty with
  | A_typ (Typ_aux (Typ_app (id, args), _)) when id_to_str id = "bits" ->
      Hashtbl.add state.bitv_synonyms (id_to_str abbrev)
        (sail_bitv_size_to_int64 (List.nth args 0))
  | _ -> ()

let add_bitv2enum_entry tbl enum_id bitv_lit =
  let enum_name = id_to_str enum_id in
  let const = bitv_literal_to_int64 bitv_lit in
  Hashtbl.add tbl const enum_name

let to_bitv2enum_hshtbl mapping_clauses =
  let bitv2enum_tbl = Hashtbl.create (List.length mapping_clauses) in
  List.iter
    (fun cl ->
      let (MCL_aux (clause, _)) = cl in
      match clause with
      | MCL_bidir (l, r) -> (
          let (MPat_aux (left, _)) = l in
          let (MPat_aux (right, _)) = r in
          match (left, right) with
          | MPat_pat lpat, MPat_pat rpat -> (
              let (MP_aux (p1, _)) = lpat in
              let (MP_aux (p2, _)) = rpat in
              match (p1, p2) with
              | MP_id enum_id, MP_lit bitv_const ->
                  add_bitv2enum_entry bitv2enum_tbl enum_id bitv_const
              | MP_lit bitv_const, MP_id enum_id ->
                  add_bitv2enum_entry bitv2enum_tbl enum_id bitv_const
              | _ ->
                  failwith
                    "Expected an enum string to map to a bitvec literal in \
                     enum<->bitvec mapping"
            )
          | _ ->
              failwith
                "Both sides of a bidiectional enum<->bitvec mapping must be a \
                 simple pattern"
        )
      | _ ->
          failwith "Non-bidirectional enum<->bitvec mappings are not supported"
    )
    mapping_clauses;
  bitv2enum_tbl

let is_enum_bitv_mapping enum_names mapping_type_annotation =
  let (Typ_annot_opt_aux (tannot, _)) = mapping_type_annotation in
  match tannot with
  | Typ_annot_opt_some (_, Typ_aux (Typ_bidir (t1, t2), _)) -> (
      let (Typ_aux (type1, _)) = t1 in
      let (Typ_aux (type2, _)) = t2 in
      match (type1, type2) with
      | Typ_app (id1, args), Typ_id id2
        when set_contains enum_names (id_to_str id2) && id_to_str id1 = "bits"
        ->
          Some (id_to_str id2, sail_bitv_size_to_int64 (List.nth args 0))
      | Typ_id id1, Typ_app (id2, args)
        when set_contains enum_names (id_to_str id1) && id_to_str id2 = "bits"
        ->
          Some (id_to_str id1, sail_bitv_size_to_int64 (List.nth args 0))
      | _ -> None
    )
  | _ -> None

let collect_enum_bitv_mapping state _ id typ_annot clauses =
  match is_enum_bitv_mapping state.enum_names typ_annot with
  | Some (enum_name, bitv_size) ->
      Hashtbl.add state.enum_bitv_mappings_registery (id_to_str id)
        (to_bitv2enum_hshtbl clauses);
      Hashtbl.add state.bitv_synonyms enum_name bitv_size
  | _ -> ()

let print_state state =
  print_endline "Union case to args ::";
  Hashtbl.iter
    (fun case_name args ->
      print_endline "------------------Case-------------------------";
      print_endline case_name;
      List.iter
        (fun a ->
          match a with
          | Named_type name -> print_endline ("Named: " ^ name)
          | Bitvec size -> print_endline ("Bitvec: " ^ Int64.to_string size)
        )
        args;
      print_endline "------------------End Case---------------------"
    )
    state.union_case_to_args;
  print_endline "========================================";
  print_endline "Enum Names:";
  Hashtbl.iter (fun name _ -> print_endline name) state.enum_names;
  print_endline "========================================";
  Hashtbl.iter
    (fun name size ->
      print_endline
        ("Type " ^ name ^ " synonomous to bitvec of size "
       ^ Int64.to_string size
        )
    )
    state.bitv_synonyms;
  print_endline "========================================";
  Hashtbl.iter
    (fun name tbl ->
      print_endline
        ("+++++++++++ Mapping from " ^ name ^ " to bitvec @@@@@@@@@@@@@@@@@@");
      Hashtbl.iter
        (fun constant enum_str ->
          print_endline (Int64.to_string constant ^ " <---> " ^ enum_str)
        )
        tbl
    )
    state.enum_bitv_mappings_registery;
  print_endline "========================================";
  print_endline
    ("Instruction Length :" ^ Int64.to_string (Option.get state.instr_length));
  print_endline "========================================";
  List.iter

let lit_to_consequence_body _ = failwith "aAAAAAAAAAAAAAAaa"

let idstr_to_consequence_body idstr = Push (Binding idstr)

let vec_concat_to_consequence_body _ = failwith "BBBBBBBBBBBBBBBBBBBBBBBB"

let bind_args_and_create_consequences state l =
  let (MPat_aux (left, _)) = l in
  let bindings = Hashtbl.create 20 in
  match left with
  | MPat_pat p | MPat_when (p, _) -> (
      let (MP_aux (pat, _)) = p in
      match pat with
      | MP_app (union_case_id, union_case_args) ->
          let case_name = id_to_str union_case_id in
          let arg_types = Hashtbl.find state.union_case_to_args case_name in
          let arg_sizes =
            List.map
              (fun arg ->
                match arg with
                | Named_type name -> Hashtbl.find state.bitv_synonyms name
                | Bitvec size -> size
              )
              arg_types
          in
          let bodies =
            List.mapi
              (fun i a ->
                let (MP_aux (arg, _)) = a in
                match arg with
                | MP_lit lit -> lit_to_consequence_body lit
                | MP_id id ->
                    let n = id_to_str id in
                    let bitv_size = List.nth arg_sizes i in
                    Hashtbl.add bindings n bitv_size;
                    idstr_to_consequence_body n
                | MP_vector_concat slices ->
                    vec_concat_to_consequence_body slices
                | _ ->
                    failwith
                      ("Unsupported pattern in argument " ^ string_of_int i
                     ^ " to " ^ case_name
                      )
              )
              union_case_args
          in
          (bindings, (Assign_node_type case_name, bodies))
      | _ ->
          failwith
            "Left pattern of the ast decoding mapping must be a union \
             constructor"
    )

let create_conditions state r arg_bindings =
  let (MPat_aux (right, _)) = r in
  match right with
  | MPat_pat p | MPat_when (p, _) -> (
      let (MP_aux (pat, _)) = p in
      match pat with
      | MP_id id -> [Bind (Option.get state.instr_length, id_to_str id)]
      | MP_vector_concat pats ->
          List.map
            (fun p ->
              let (MP_aux (pat, _)) = p in
              match pat with
              | MP_id id ->
                  let idstr = id_to_str id in
                  let size = Hashtbl.find arg_bindings idstr in
                  Bind (size, idstr)
              | MP_typ (pat, typ) ->
                  let size =
                    match typ with
                    | Typ_aux (Typ_app (id, args), _)
                      when id_to_str id = "bitvector" ->
                        sail_bitv_size_to_int64 (List.nth args 0)
                    | _ -> failwith "Type annotation cant be non-bitvec"
                  in
                  let idstr =
                    match pat with
                    | MP_aux (MP_id id, _) -> id_to_str id
                    | _ -> failwith "Cant annotate non-id"
                  in
                  Bind (size, idstr)
              | MP_lit lit ->
                  let (L_aux (bv_lit, _)) = lit in
                  let size, const =
                    match bv_lit with
                    | L_hex num_str ->
                        ( Int64.of_int (String.length num_str),
                          Int64.of_string num_str
                        )
                    | L_bin num_str ->
                        ( Int64.of_int (String.length num_str * 4),
                          Int64.of_string num_str
                        )
                    | _ ->
                        failwith
                          "Unsupported literal, only bitvec literals make \
                           sense here"
                  in
                  Assert (size, const)
              | MP_app (id, args) ->
                  let mapping_name = id_to_str id in
                  let enum_name =
                    match args with
                    | [MP_aux (MP_id i, _)] -> id_to_str i
                    | _ ->
                        failwith
                          "Unsupported mapping pattern, multiple arguments are \
                           not supported"
                  in
                  let bv_to_enum =
                    Hashtbl.find state.enum_bitv_mappings_registery mapping_name
                  in
                  let size = Hashtbl.find arg_bindings enum_name in
                  Map_bind (size, bv_to_enum, enum_name)
              | _ -> failwith ""
            )
            pats
      | _ -> failwith "-"
    )

let calculate_instr_length typ =
  let errmsg = "Cant calculate instruction length from ast decode mapping" in

  match typ with
  | Typ_aux (Typ_bidir (t1, t2), _) -> (
      let (Typ_aux (type1, _)) = t1 in
      let (Typ_aux (type2, _)) = t2 in
      match (type1, type2) with
      | Typ_app (id, args), _ when id_to_str id = "bitvector" ->
          sail_bitv_size_to_int64 (List.nth args 0)
      | _, Typ_app (id, args) when id_to_str id = "bitvector" ->
          sail_bitv_size_to_int64 (List.nth args 0)
      | _ -> failwith errmsg
    )
  | _ -> failwith errmsg

let calculate_instr_len state _ id _ typ =
  if id_to_str id = ast_decode_mapping then (
    assert (Option.is_none state.instr_length);
    state.instr_length <- Some (calculate_instr_length typ)
  )

let gen_decode_rule state _ id tannot left right =
  if id_to_str id = ast_decode_mapping then (
    let bindings, consequences = bind_args_and_create_consequences state left in
    let conditions = create_conditions state right bindings in
    state.decode_rules <- (conditions, consequences) :: state.decode_rules
  )

let gen_decoder ast =
  let state =
    {
      union_case_to_args = Hashtbl.create 50;
      enum_names = Hashtbl.create 100;
      bitv_synonyms = Hashtbl.create 50;
      enum_bitv_mappings_registery = Hashtbl.create 50;
      decode_rules = [];
      instr_length = None;
    }
  in
  let decoder_gen_processor =
    {
      default_processor with
      process_union_clause = assoc_clause_with_args;
      process_abbrev = collect_bitvec_abbreviations;
      process_enum = collect_enums;
      process_mapping = collect_enum_bitv_mapping;
      process_mapping_bidir_clause = gen_decode_rule;
      process_val = calculate_instr_len;
    }
  in
  foreach_node ast decoder_gen_processor state;
  state.decode_rules <- List.rev state.decode_rules;
  state.decode_rules

(***********************************************************************************************)
(************************** Decoder transformation to imperative logic *************************)
(***********************************************************************************************)

let gen_assert_boolean_exprs offsets (i, rule_cond) =
  match rule_cond with
  | Assert (_, bval) ->
      let starting = List.nth offsets i in
      let ending = List.nth offsets (i + 1) in
      Some (Is_eq (Binstr_slice (starting, ending), bval))
  | _ -> None

let gen_bind_stmts offsets (i, rule_cond) =
  match rule_cond with
  | Bind (_, var) ->
      Some
        (Init (var, Binstr_slice (List.nth offsets i, List.nth offsets (i + 1))))
  | _ -> None

let calculate_offsets conds =
  let rev_offsets =
    List.fold_left
      (fun roffs cond ->
        match cond with
        | Assert (len, _) | Bind (len, _) | Map_bind (len, _, _) -> (
            match roffs with
            | curr :: rest -> Int64.add (Int64.sub len 1L) curr :: roffs
            | [] -> failwith "UNREACHABLE"
          )
      )
      [0L] conds
  in
  List.rev rev_offsets

let curr_auto_var = ref 0

let gen_var () =
  let v = "_" ^ string_of_int !curr_auto_var in
  curr_auto_var := !curr_auto_var + 1;
  v

let gen_mapbind_stmt offsets (i, cond) body =
  match cond with
  | Assert (_, _) | Bind (_, _) -> body
  | Map_bind (_, map_tbl, var_name) ->
      let values =
        Hashtbl.fold
          (fun bv_val enum_val vs -> (bv_val, enum_val) :: vs)
          map_tbl []
      in
      let slice = Binstr_slice (List.nth offsets i, List.nth offsets (i + 1)) in
      let switch = Switch_assign (var_name, slice, values) in
      Block [switch; If (Is_enum_var_valid var_name, body)]

let gen_stmt_from_conditions conds conseq_stmts =
  let offsets = calculate_offsets conds in
  let indices = List.mapi (fun i _ -> i) conds in
  let conds_indexed = List.combine indices conds in
  let assert_exprs =
    List.filter_map (gen_assert_boolean_exprs offsets) conds_indexed
  in
  let bind_stmts = List.filter_map (gen_bind_stmts offsets) conds_indexed in
  let mapbind_stmt =
    List.fold_right (gen_mapbind_stmt offsets) conds_indexed
      (Block (List.append bind_stmts conseq_stmts))
  in
  match assert_exprs with
  | [] -> mapbind_stmt
  | _ -> If (And assert_exprs, mapbind_stmt)

let gen_stmt_from_consequences consequences =
  let head, bodies = consequences in
  let (Assign_node_type case_name) = head in
  let body_stmts =
    List.map
      (fun body ->
        match body with
        | Push v -> Set_ast_next_case_member (Some v, None)
        | Concat_push vs ->
            let literals =
              List.map
                (fun v ->
                  match v with
                  | Bv_const bvc -> Literal bvc
                  | _ ->
                      failwith
                        "Type Error: concat undefined for non-bitvec values"
                )
                vs
            in
            Set_ast_next_case_member (None, Some (Concat literals))
      )
      bodies
  in
  List.append (Set_ast_case case_name :: body_stmts) [Ret_ast]

let gen_stmt_from_rule rule =
  let conditions, consequences = rule in
  let conseq_stmts = gen_stmt_from_consequences consequences in
  gen_stmt_from_conditions conditions conseq_stmts

let gen_decode_proc decoder = Proc (Block (List.map gen_stmt_from_rule decoder))
