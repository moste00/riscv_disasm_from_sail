open Libsail
open Ast
open Ast_defs
open Sail_ast_processor

let id_to_str id =
  let (Id_aux (i, _)) = id in
  match i with
  | Id s -> s
  | Operator _ -> failwith "Operator identifiers are not supported"

let foreach_fundef node proc =
  let (FD_aux (child, _)) = node in
  let (FD_function (_, _, clauses)) = child in
  List.iter
    (fun c ->
      let (FCL_aux (clause, _)) = c in
      let (FCL_funcl (id, pexp)) = clause in
      proc.process_function_clause clause id pexp
    )
    clauses

let foreach_mapdef node proc =
  let (MD_aux (child, _)) = node in
  let (MD_mapping (c1, _, _)) = child in
  let (Id_aux (c2, _)) = c1 in
  let (Id id) = c2 in
  ()

let foreach_literal node proc =
  let (L_aux (child, _)) = node in
  match child with L_hex _ -> () | _ -> ()

let foreach_expaux node proc =
  match node with
  | E_lit child -> foreach_literal child proc
  | E_app (id, args) -> print_endline (id_to_str id)
  | _ -> ()

let foreach_exp node proc =
  match node with E_aux (child, _) -> foreach_expaux child proc

let foreach_letbind_aux node proc =
  match node with LB_val (_, child) -> foreach_exp child proc

let foreach_let node proc =
  match node with LB_aux (child, _) -> foreach_letbind_aux child proc

let foreach_typedef_aux node proc =
  match node with
  | TD_abbrev (id, typquant, arg) ->
      proc.process_typedef node id;
      proc.process_abbrev node id typquant arg
  | TD_record (id, typquant, members, flg) ->
      proc.process_typedef node id;
      proc.process_record node id typquant members flg
  | TD_variant (id, typquant, type_unions, flg) ->
      proc.process_typedef node id;
      proc.process_union node id typquant type_unions flg
  | TD_enum (id, cases, flg) ->
      proc.process_typedef node id;
      proc.process_enum node id cases flg
  | TD_bitfield (id, typ, members) ->
      proc.process_typedef node id;
      proc.process_bitfield node id typ members

let foreach_typedef node proc =
  match node with TD_aux (child, _) -> foreach_typedef_aux child proc

let foreach_defaux node proc =
  match node with
  | DEF_type child -> foreach_typedef child proc
  | DEF_let child -> foreach_let child proc
  | DEF_mapdef child -> foreach_mapdef child proc
  | DEF_fundef child -> foreach_fundef child proc
  | _ -> ()

let foreach_def node proc =
  match node with DEF_aux (child, _) -> foreach_defaux child proc

let foreach_defs node proc =
  List.iter (fun def -> foreach_def def proc) node.defs

let foreach_node root processor = foreach_defs root processor
