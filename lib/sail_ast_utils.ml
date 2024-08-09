open Libsail
open Ast
open Lexing

let stringify_lex_pos start ending =
  if start.pos_fname = ending.pos_fname then
    "file " ^ start.pos_fname ^ " @ [line: "
    ^ string_of_int start.pos_lnum
    ^ ", column: "
    ^ string_of_int start.pos_cnum
    ^ "]..." ^ "[line: "
    ^ string_of_int ending.pos_lnum
    ^ ", column: "
    ^ string_of_int ending.pos_cnum
    ^ "]"
  else "multi-file element"

let stringify_sail_source_loc loc =
  let open Parse_ast in
  match loc with
  | Range (start, ending) -> stringify_lex_pos start ending
  | _ -> "<unprintable>"

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

let convert_bitv_size_to_int ?(throw_on_unsupported_size_exprs = true) size =
  let (A_aux (a, _)) = size in
  match a with
  | A_nexp (Nexp_aux (nexp, loc)) -> (
      match nexp with
      | Nexp_constant sz -> Nat_big_num.to_int sz
      | _ ->
          if throw_on_unsupported_size_exprs then
            failwith
              ("Unsupported size expression in call to built-in type \
                constructor 'bits', only constants are supported @ "
              ^ stringify_sail_source_loc loc
              );
          -1
    )
  | _ -> failwith "UNREACHABLE"

let sail_bitv_size_to_int =
  convert_bitv_size_to_int ~throw_on_unsupported_size_exprs:true
let sail_bitv_size_to_int_noexn =
  convert_bitv_size_to_int ~throw_on_unsupported_size_exprs:false
