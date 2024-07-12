open Sail_ast_processor
open Sail_ast_foreach
open Sail_ast_utils

open Libsail
open Ast

open Clike_typedef
open Constants

module StringMap = Map.Make (String)

let collect_types ast =
  let registery : (string * type_def_aux) list ref = ref [] in
  let types_collector =
    {
      default_processor with
      process_typedef =
        (fun _ node typename ->
          registery := (id_to_str typename, node) :: !registery
        );
    }
  in
  foreach_node ast types_collector ();
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

let int64_to_clike_bitv_size size =
  if size = 1L then Clike_bit
  else if size <= 8L then Clike_byte
  else if size <= 16L then Clike_word
  else if size <= 32L then Clike_dword
  else if size <= 64L then Clike_qword
  else
    failwith
      ("size constant too big, can't represent bitvec of size "
     ^ Int64.to_string size
      )

let gen_clike_builtin_from_bitvec_size_expr name arg =
  Clike_builtin (name, int64_to_clike_bitv_size (sail_bitv_size_to_int64 arg))

let gen_clike_type_from_app name constructor args =
  match constructor with
  | "bits" -> gen_clike_builtin_from_bitvec_size_expr name (List.nth args 0)
  | _ -> failwith ("Unsupported type constructor call " ^ constructor)

let rec gen_clike_type name type_reg typ =
  let (Typ_aux (typ_aux, _)) = typ in
  match typ_aux with
  | Typ_id id -> gen_clike_type_from_id name type_reg id
  | Typ_app (id, args) -> gen_clike_type_from_app name (id_to_str id) args
  | Typ_tuple types -> gen_clike_type_from_tuple name type_reg types
  | _ -> invalid_arg "TODO 1"

and gen_clike_type_from_id name type_reg id =
  let typ_def = StringMap.find (id_to_str id) type_reg in
  match typ_def with
  | TD_abbrev (_, _, typarg) -> gen_clike_type_from_abbrev name type_reg typarg
  | TD_enum (_, case_names, _) -> gen_clike_type_from_enum name case_names
  | _ -> invalid_arg "TODO 2"

and gen_clike_type_from_abbrev name type_reg typarg =
  match typarg with
  | A_aux (A_typ typ, _) -> gen_clike_type name type_reg typ
  | _ -> failwith "Type arguments other than A_typ are not supported"

and gen_clike_type_from_tuple name type_reg types =
  let clike_types = List.map (gen_clike_type "" type_reg) types in
  Clike_struct ("", name, clike_types)

and gen_clike_type_from_enum name case_names =
  Clike_enum ("", name, List.map id_to_str case_names)

let gen_clike_typedef ast_typename type_reg =
  let ast_type = StringMap.find ast_typename type_reg in
  let case_names, case_bodies = split_union_into_names_and_bodies ast_type in
  let clike_bodies =
    List.mapi
      (fun i t ->
        let name = String.lowercase_ascii (List.nth case_names i) in
        gen_clike_type name type_reg t
      )
      case_bodies
  in
  Clike_struct
    ( ast_typename,
      "",
      [
        Clike_enum ("", ast_typename ^ generated_ast_enum_suffix, case_names);
        Clike_union
          ("", ast_typename ^ generated_ast_payload_suffix, clike_bodies);
      ]
    )

let gen_def ast =
  let type_reg = collect_types ast in
  gen_clike_typedef ast_sail_def_name type_reg

let collect_case_names_and_member_names ast =
  let constructor_name_to_member_names = Hashtbl.create 50 in
  let names_collector =
    {
      default_processor with
      process_function_clause =
        (fun _ _ name (Pat_aux (pattern_expr, _)) ->
          let name_str = id_to_str name in
          if name_str = ast_sail_destructuring_function then (
            match pattern_expr with
            | Pat_exp (P_aux (pattern, _), _)
            | Pat_when (P_aux (pattern, _), _, _) -> (
                match pattern with
                | P_app (case_name, [P_aux (P_tuple member_list, _)]) ->
                    let case_name_as_str = id_to_str case_name in
                    let member_str_list =
                      List.map
                        (fun m ->
                          match m with
                          | P_aux (P_id i, _) -> id_to_str i
                          | _ ->
                              failwith
                                "Naming nameless ast members failed: expected \
                                 all args to the union constructor to be \
                                 identifiers"
                        )
                        member_list
                    in
                    Hashtbl.add constructor_name_to_member_names
                      (String.lowercase_ascii case_name_as_str)
                      member_str_list
                | _ ->
                    failwith
                      "Naming nameless ast members failed: Expected all \
                       clauses of the ast destructuring function to have a \
                       single union constructor call"
              )
          )
        );
    }
  in
  foreach_node ast names_collector ();
  constructor_name_to_member_names

let name_members members names_registery =
  List.map
    (fun member ->
      match member with
      | Clike_struct (t_name, case_name, nameless_sub_members) ->
          let member_names = Hashtbl.find names_registery case_name in
          let named_sub_members =
            List.mapi
              (fun i sub_member ->
                let sub_member_name = List.nth member_names i in
                match sub_member with
                | Clike_builtin (_, data) ->
                    Clike_builtin (sub_member_name, data)
                | Clike_enum (tname, _, data) ->
                    Clike_enum (tname, sub_member_name, data)
                | Clike_union (tname, _, data) ->
                    Clike_union (tname, sub_member_name, data)
                | Clike_struct (tname, _, data) ->
                    Clike_struct (tname, sub_member_name, data)
              )
              nameless_sub_members
          in
          Clike_struct (t_name, case_name, named_sub_members)
      | other -> other
    )
    members

let name_nameless_defs ast clike_typedef =
  let case_names_registery = collect_case_names_and_member_names ast in
  match clike_typedef with
  | Clike_struct
      ( tname,
        name,
        [
          Clike_enum (e_tname, e_name, data1);
          Clike_union (u_tname, u_name, members);
        ]
      ) ->
      let named_members = name_members members case_names_registery in
      Clike_struct
        ( tname,
          name,
          [
            Clike_enum (e_tname, e_name, data1);
            Clike_union (u_tname, u_name, named_members);
          ]
        )
  | _ -> failwith "UNREACHABLE"
