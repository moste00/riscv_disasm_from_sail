open Libsail
open Ast

let id_to_str id =
  let (Id_aux (i, _)) = id in
  match i with
  | Id s -> s
  | Operator _ -> failwith "Operator identifiers are not supported"

let sail_bitv_size_to_int64 size =
  let (A_aux (a, _)) = size in
  match a with
  | A_nexp (Nexp_aux (nexp, _)) -> (
      match nexp with
      | Nexp_constant sz -> Nat_big_num.to_int64 sz
      | _ ->
          failwith
            "Unsupported size expression in call to built-in type constructor \
             'bits', only constants are supported"
    )
  | _ -> failwith "UNREACHABLE"

let bitv_literal_to_int64 bitv_lit =
  let (L_aux (lit, _)) = bitv_lit in
  match lit with
  | L_hex lit_str | L_bin lit_str -> Int64.of_string lit_str
  | _ -> failwith "Expected a bitvec literal, found neither L_hex nor L_bin"
