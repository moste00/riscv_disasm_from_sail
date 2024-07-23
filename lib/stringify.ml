open Clike_typedef
open Decode_procedure
open Constants

let mk_indentation lvl =
  if lvl = 0 then ""
  else (
    let buff = Buffer.create (lvl * 2) in
    for _ = 1 to lvl do
      Buffer.add_string buff "  " (* 2 spaces *)
    done;
    Buffer.contents buff
  )

let stringify_clike_builin name builtin =
  match builtin with
  | Clike_bitfield size ->
      let pow2_rounded_size =
        if size < 8L then 8
        else if size < 16L then 16
        else if size < 32L then 32
        else 64
      in
      "uint"
      ^ string_of_int pow2_rounded_size
      ^ "_t " ^ name ^ " : " ^ Int64.to_string size ^ " ;"
  | Clike_byte -> "uint8_t " ^ name ^ " ;"
  | Clike_word -> "uint16_t " ^ name ^ " ;"
  | Clike_dword -> "uint32_t " ^ name ^ " ;"
  | Clike_qword -> "uint64_t " ^ name ^ " ;s"

let rec stringify_clike_typedef ?(indentation_lvl = 0) clike_typdef =
  let indent = mk_indentation (indentation_lvl + 1) in
  let ind = mk_indentation indentation_lvl in
  let stringifier m =
    stringify_clike_typedef m ~indentation_lvl:(indentation_lvl + 1)
  in
  match clike_typdef with
  | Clike_enum (typname, name, constants) ->
      "enum " ^ typname ^ " {\n" ^ indent
      ^ String.concat (",\n" ^ indent) constants
      ^ "\n" ^ ind ^ "} " ^ name ^ ";\n"
  | Clike_struct (typname, name, members) ->
      let members_as_str = List.map stringifier members in
      "struct " ^ typname ^ " {\n" ^ indent
      ^ String.concat ("\n" ^ indent) members_as_str
      ^ "\n" ^ ind ^ "} " ^ name ^ ";\n"
  | Clike_union (typname, name, members) ->
      let members_as_str = List.map stringifier members in
      "union " ^ typname ^ " {\n" ^ indent
      ^ String.concat ("\n" ^ indent) members_as_str
      ^ "\n" ^ ind ^ "} " ^ name ^ ";\n"
  | Clike_builtin (name, bitvec) -> stringify_clike_builin name bitvec

type decproc_stringification_state = {
  mutable current_case_name : string;
  mutable current_case_members : string list;
  constructor_names_to_member_names : (string, string list) Hashtbl.t;
}

let stringify_bv e =
  match e with
  | Literal l -> l
  | Binstr_slice (i_1, i_2) ->
      let i1, i2 = (Int64.to_int i_1, Int64.to_int i_2) in
      let mask =
        0 :: List.init 63 (fun x -> x + 1)
        |> List.map (fun i -> if i >= i1 && i <= i2 then "1" else "0")
        |> List.rev |> String.concat ""
      in
      let masking_expr = binary_stream_c_parameter ^ " & 0b" ^ mask in
      if i1 <> 0 then "(" ^ masking_expr ^ ")" ^ ">>" ^ string_of_int i1
      else masking_expr
  | _ -> "TODO_BV"

let rec stringify_bool b =
  match b with
  | Is_eq (bv_expr, valu) -> stringify_bv bv_expr ^ " == " ^ valu
  | Is_enum_var_valid var -> var ^ " != 0xFFFF" ^ "FFFF" ^ "FFFF" ^ "FFFF"
  | And exprs ->
      "(" ^ String.concat ") && (" (List.map stringify_bool exprs) ^ ")"

let rec stringify_stmt str_state stmt =
  match stmt with
  | Init (var, expr) -> "uint64_t " ^ var ^ " = " ^ stringify_bv expr ^ " ;"
  | If (cond, body) ->
      "if (" ^ stringify_bool cond ^ ") { \n"
      ^ stringify_stmt str_state body
      ^ " \n}"
  | Switch_assign (var, exp, cases) ->
      let var_decl =
        "uint64_t " ^ var ^ " = 0xFFFF" ^ "FFFF" ^ "FFFF" ^ "FFFF ;\n"
      in
      let switch_start = "switch (" ^ stringify_bv exp ^ ") { \n" in
      let cases =
        List.map
          (fun (bval, enmval) ->
            "case " ^ bval ^ ": \n" ^ var ^ " = " ^ enmval ^ " ; \n break; \n"
          )
          cases
      in
      var_decl ^ switch_start ^ String.concat "" cases ^ " \n }"
  | Set_ast_case case ->
      str_state.current_case_name <- String.lowercase_ascii case;
      str_state.current_case_members <-
        Hashtbl.find str_state.constructor_names_to_member_names
          str_state.current_case_name;
      ast_c_parameter ^ "->"
      ^ (ast_sail_def_name ^ generated_ast_enum_suffix)
      ^ " = " ^ case ^ " ;"
  | Set_ast_next_case_member member_rhs ->
      let member_c_name =
        match str_state.current_case_members with
        | first :: rest ->
            str_state.current_case_members <- rest;
            first
        | _ -> failwith "UNREACHABLE"
      in
      let rhs_string =
        match member_rhs with
        | Val v -> (
            match v with
            | Bool_const c -> if c then "1" else "0"
            | Bv_const s | Binding s | Enum_lit s -> s
          )
        | Exp bv_expr -> stringify_bv bv_expr
      in
      ast_c_parameter ^ "->"
      ^ (ast_sail_def_name ^ generated_ast_payload_suffix)
      ^ "." ^ str_state.current_case_name ^ "." ^ member_c_name ^ " = "
      ^ rhs_string ^ ";"
  | Ret_ast -> "return " ^ ast_c_parameter ^ ";"
  | Block stmts -> String.concat "\n" (List.map (stringify_stmt str_state) stmts)

let stringify_decode_procedure (Proc stmt) case_names_to_members =
  let initial_stringficiation_state =
    {
      current_case_name = "";
      current_case_members = [];
      constructor_names_to_member_names = case_names_to_members;
    }
  in
  stringify_stmt initial_stringficiation_state stmt
