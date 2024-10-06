open Libsail
open Ast
open Ast_defs
open Sail_ast_processor

let foreach_val node proc state =
  let (VS_aux (child, _)) = node in
  let (VS_val_spec (type_scheme, id, _)) = child in
  let (TypSchm_aux (tschm, _)) = type_scheme in
  let (TypSchm_ts (quantifier, typ)) = tschm in
  proc.process_val state child id quantifier typ

let foreach_fundef node proc state =
  let (FD_aux (child, _)) = node in
  let (FD_function (_, _, clauses)) = child in
  List.iter
    (fun c ->
      let (FCL_aux (clause, _)) = c in
      let (FCL_funcl (id, pexp)) = clause in
      proc.process_function_clause state clause id pexp
    )
    clauses

let foreach_mapdef node proc state =
  let (MD_aux (mapping, _)) = node in
  let (MD_mapping (id, tannot, clauses)) = mapping in
  proc.process_mapping state mapping id tannot clauses;
  List.iter
    (fun cl ->
      let (MCL_aux (clause, _)) = cl in
      match clause with
      | MCL_bidir (pat1, pat2) ->
          proc.process_mapping_bidir_clause state clause id tannot pat1 pat2
      | _ -> ()
    )
    clauses

let foreach_let node proc state = match node with LB_aux (child, _) -> ()

let foreach_typedef_aux node proc state =
  match node with
  | TD_abbrev (id, typquant, arg) ->
      proc.process_typedef state node id;
      proc.process_abbrev state node id typquant arg
  | TD_record (id, typquant, members, flg) ->
      proc.process_typedef state node id;
      proc.process_record state node id typquant members flg
  | TD_variant (id, typquant, clauses, flg) ->
      proc.process_typedef state node id;
      proc.process_union state node id typquant clauses flg;
      List.iter
        (fun tu ->
          let (Tu_aux (type_union, _)) = tu in
          let (Tu_ty_id (typ, clause_id)) = type_union in
          proc.process_union_clause state type_union id clause_id typ
        )
        clauses
  | TD_enum (id, cases, flg) ->
      proc.process_typedef state node id;
      proc.process_enum state node id cases flg
  | TD_bitfield (id, typ, members) ->
      proc.process_typedef state node id;
      proc.process_bitfield state node id typ members

let foreach_typedef node proc state =
  match node with TD_aux (child, _) -> foreach_typedef_aux child proc state

let foreach_defaux node proc state =
  match node with
  | DEF_type child -> foreach_typedef child proc state
  | DEF_let child -> foreach_let child proc state
  | DEF_mapdef child -> foreach_mapdef child proc state
  | DEF_fundef child -> foreach_fundef child proc state
  | DEF_val child -> foreach_val child proc state
  | _ -> ()

let foreach_def node proc state =
  match node with DEF_aux (child, _) -> foreach_defaux child proc state

let foreach_defs node proc state =
  List.iter (fun def -> foreach_def def proc state) node.defs

let foreach_node root processor init_state =
  foreach_defs root processor init_state
