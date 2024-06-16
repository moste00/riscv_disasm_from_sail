open Sail_ast_processor
open Sail_ast_foreach
open Libsail
open Ast
open Clike_typedef

module StringMap = Map.Make (String)
type type_registery = type_def_aux StringMap.t

let id_to_str id =
  let (Id_aux (i, _)) = id in
  match i with
  | Id s -> s
  | Operator _ -> failwith "Operator identifiers not supported"

let collect_types ast =
  let registery : (string * type_def_aux) list ref = ref [] in
  let types_collector =
    {
      default_processor with
      process_typedef =
        (fun node typename ->
          registery := (id_to_str typename, node) :: !registery
        );
    }
  in
  let () = foreach_node ast types_collector in
  StringMap.of_seq (List.to_seq !registery)

let split_union_into_names_and_bodies (union : type_def_aux) =
  let split_single_union_case_into_name_and_body union_case names_and_bodies =
    let (Tu_aux (Tu_ty_id (typ, id), _)) = union_case in
    let name = id_to_str id in
    let names, bodies = names_and_bodies in
    (name :: names, typ :: bodies)
  in
  match union with
  | TD_variant (_, _, cases, _) ->
      List.fold_right split_single_union_case_into_name_and_body cases ([], [])
  | _ -> invalid_arg "Expected a TD_variant, found non-variant type"

let bignum_to_clike_bitv_size bignum =
  let size = Nat_big_num.to_int bignum in
  if size = 1 then Clike_bit
  else if size <= 8 then Clike_byte
  else if size <= 16 then Clike_word
  else if size <= 32 then Clike_dword
  else if size <= 64 then Clike_qword
  else
    failwith
      ("size constant too big, can't represent bitvec of size "
     ^ string_of_int size
      )

let gen_clike_builtin_from_bitvec_size_expr arg =
  let (A_aux (a, _)) = arg in
  match a with
  | A_nexp (Nexp_aux (nexp, _)) -> (
      match nexp with
      | Nexp_constant size -> Clike_builtin ("", bignum_to_clike_bitv_size size)
      | _ ->
          failwith
            "Unsupported size expression in call to built-in type constructor \
             'bits', Only constants are supported"
    )
  | _ -> failwith "UNREACHABLE"

let gen_clike_type_from_app name args =
  match name with
  | "bits" when List.length args = 1 ->
      gen_clike_builtin_from_bitvec_size_expr (List.nth args 0)
  | "bits" ->
      failwith
        ("Unrecognized arity when calling the built-in type constructor \
          'bits': found "
        ^ string_of_int (List.length args)
        ^ " arguments, expected 1"
        )
  | _ -> failwith ("Unsupported type constructor call " ^ name)

let rec gen_clike_type type_reg typ =
  let (Typ_aux (typ_aux, _)) = typ in
  match typ_aux with
  | Typ_id id -> (
      let typ_def = StringMap.find (id_to_str id) type_reg in
      match typ_def with
      | TD_abbrev (_, _, typarg) -> gen_clike_type_from_abbrev type_reg typarg
      | _ -> invalid_arg "TODO"
    )
  | Typ_app (id, args) -> gen_clike_type_from_app (id_to_str id) args
  | _ -> invalid_arg "TODO"

and gen_clike_type_from_abbrev type_reg typarg =
  match typarg with
  | A_aux (A_typ typ, _) -> gen_clike_type type_reg typ
  | _ -> failwith "Type arguments other than A_typ are not supported"

let gen_clike_typedef ast_typename type_reg =
  let ast_type = StringMap.find ast_typename type_reg in
  let case_names, case_bodies = split_union_into_names_and_bodies ast_type in
  let clike_bodies = List.map (gen_clike_type type_reg) case_bodies in
  Clike_struct
    ( ast_typename,
      "",
      [
        Clike_enum ("", ast_typename ^ "_node_type", case_names);
        Clike_union ("", ast_typename ^ "_node", clike_bodies);
      ]
    )

let gen_def ast_typename ast =
  let type_reg = collect_types ast in
  gen_clike_typedef ast_typename type_reg
