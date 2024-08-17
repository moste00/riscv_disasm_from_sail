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

let binary_chars_to_hex_digit b3 b2 b1 b0 = 
  match (b3, b2, b1, b0) with 
  | ('0', '0', '0', '0') -> "0"
  | ('0', '0', '0', '1') -> "1"
  | ('0', '0', '1', '0') -> "2"
  | ('0', '0', '1', '1') -> "3"
  | ('0', '1', '0', '0') -> "4"
  | ('0', '1', '0', '1') -> "5"
  | ('0', '1', '1', '0') -> "6"
  | ('0', '1', '1', '1') -> "7"
  | ('1', '0', '0', '0') -> "8"
  | ('1', '0', '0', '1') -> "9"
  | ('1', '0', '1', '0') -> "A"
  | ('1', '0', '1', '1') -> "B"
  | ('1', '1', '0', '0') -> "C"
  | ('1', '1', '0', '1') -> "D"
  | ('1', '1', '1', '0') -> "E"
  | ('1', '1', '1', '1') -> "F"
  | _ -> failwith "UNREACHABLE"

let binary_str_to_hex_str s = 
  let slen = String.length s in
  let slen_mod4 = slen mod 4 in
  let padding = if slen_mod4 = 0 then "" else List.init (4 - slen_mod4) (fun _ -> "0") |> String.concat "" in 
  let s_padded_4 = padding ^ s in 
  let padlen = String.length s_padded_4 in
  let hexstr = Buffer.create (padlen/4) in 
  let i = ref 0 in
  while !i < padlen do
    let hex_digit = binary_chars_to_hex_digit s_padded_4.[!i] s_padded_4.[!i+1] s_padded_4.[!i+2] s_padded_4.[!i+3] in
    Buffer.add_string hexstr hex_digit;
    i := !i + 4
  done;
  Buffer.contents hexstr
  
let bitv_literal_to_str bitv_lit =
  let (L_aux (lit, _)) = bitv_lit in
  match lit with
  | L_hex lit_str -> "0x" ^ lit_str
  | L_bin lit_str -> "0x" ^ (binary_str_to_hex_str lit_str)
  | _ -> failwith "Expected a bitvec literal, found neither L_hex nor L_bin"

let bitv_literal_size bitv_lit = 
  let (L_aux (lit, _)) = bitv_lit in
  match lit with
  | L_hex lit_str -> (String.length lit_str) * 4
  | L_bin lit_str -> String.length lit_str
  | _ -> failwith "Expected a bitvec literal, found neither L_hex nor L_bin"