open Libsail
open Ast
open Lexing

let stringify_lex_pos start ending =
  if start.pos_fname = ending.pos_fname then
    start.pos_fname ^ " @ ["
    ^ string_of_int start.pos_lnum
    ^ ","
    ^ string_of_int start.pos_cnum
    ^ "]:" ^ "["
    ^ string_of_int ending.pos_lnum
    ^ ","
    ^ string_of_int ending.pos_cnum
    ^ "]"
  else "cross-file element"

let stringify_sail_source_loc loc =
  let open Parse_ast in
  match loc with
  | Range (start, ending) -> stringify_lex_pos start ending
  | _ -> failwith "TODO_SRC_LOC"

let id_to_str id =
  let (Id_aux (i, loc)) = id in
  match i with
  | Id s -> s
  | Operator _ ->
      failwith
        ("Operator identifiers are not supported @ "
        ^ stringify_sail_source_loc loc
        )

let id_to_str_noexn id =
  let (Id_aux (i, loc)) = id in
  match i with Id s -> s | Operator _ -> ""

let sail_bitv_size_to_int size =
  let (A_aux (a, _)) = size in
  match a with
  | A_nexp (Nexp_aux (nexp, _)) -> (
      match nexp with
      | Nexp_constant sz -> Nat_big_num.to_int sz
      | _ ->
          failwith
            "Unsupported size expression in call to built-in type constructor \
             'bits', only constants are supported"
    )
  | _ -> failwith "UNREACHABLE"
