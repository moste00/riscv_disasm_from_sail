open Clike_typedef
open Decoder
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
        if size < 8 then 8
        else if size < 16 then 16
        else if size < 32 then 32
        else 64
      in
      "uint"
      ^ string_of_int pow2_rounded_size
      ^ "_t " ^ name ^ " /* bits : " ^ string_of_int size ^ " */;"
  | Clike_byte -> "uint8_t " ^ name ^ ";"
  | Clike_word -> "uint16_t " ^ name ^ ";"
  | Clike_dword -> "uint32_t " ^ name ^ ";"
  | Clike_qword -> "uint64_t " ^ name ^ ";"

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
  | Clike_void -> ""
  | Clike_typename (typname, name) -> typname ^ " " ^ name ^ ";"

let stringify_typdef typdef =
  "enum {false = 0, true = 1}; \n" ^ stringify_clike_typedef typdef

type decproc_stringification_state = {
  mutable current_case_name : string;
  mutable current_case_members : string list;
  constructor_names_to_member_names : (string, string list) Hashtbl.t;
  currently_defined_bv_sizes : (string, int) Hashtbl.t;
  builtin_cases : string list;
}

let rec bv_len vars bv =
  match bv with
  | Literal lit ->
      if lit.[1] = 'b' then String.length lit - 2 (* account for the 0b *)
      else (String.length lit - 2) * 4
  | Binstr_slice (i1, i2) -> i2 - i1
  | Concat bvs -> List.fold_left ( + ) 0 (List.map (bv_len vars) bvs)
  | Id name -> Hashtbl.find vars.currently_defined_bv_sizes name

let rec stringify_bv vars e =
  match e with
  | Literal l -> l
  | Binstr_slice (i1, i2) ->
      let mask =
        0 :: List.init 63 (fun x -> x + 1)
        |> List.map (fun i -> if i >= i1 && i < i2 then "1" else "0")
        |> List.rev |> String.concat ""
      in
      let masking_expr = binary_stream_c_parameter ^ " & 0b" ^ mask in
      if i1 <> 0 then "(" ^ masking_expr ^ ")" ^ ">>" ^ string_of_int i1
      else masking_expr
  | Concat bvs ->
      let offsets =
        List.rev
          (List.fold_left
             (fun lengths bv ->
               match lengths with
               | length_prev :: rest ->
                   let length = bv_len vars bv in
                   (length + length_prev) :: lengths
               | _ -> failwith "UNREACHABLE"
             )
             [0] bvs
          )
      in
      let shifted_exprs =
        List.mapi
          (fun i bv ->
            stringify_bv vars bv ^ " << " ^ string_of_int (List.nth offsets i)
          )
          bvs
      in
      "(" ^ String.concat ") | (" (List.rev shifted_exprs) ^ ")"
  | Id name -> name

let rec stringify_bool vars b =
  match b with
  | Is_eq (bv_expr, valu) -> stringify_bv vars bv_expr ^ " == " ^ valu
  | Is_enum_var_valid var -> var ^ " != 0xFFFF" ^ "FFFF" ^ "FFFF" ^ "FFFF"
  | Is_struct_var_valid var -> var ^ "_is_valid == 1"
  | And exprs ->
      "(" ^ String.concat ") && (" (List.map (stringify_bool vars) exprs) ^ ")"

let rec stringify_stmt ?(indentation_lvl = 0) str_state stmt =
  let indent = mk_indentation (indentation_lvl + 1) in
  let ind = mk_indentation indentation_lvl in
  let indentation_lvl = indentation_lvl + 1 in
  match stmt with
  | Init (var, expr) ->
      Hashtbl.add str_state.currently_defined_bv_sizes var
        (bv_len str_state expr);
      ind ^ "uint64_t " ^ var ^ " = " ^ stringify_bv str_state expr ^ " ;\n"
  | If (cond, body) ->
      ind ^ "if ("
      ^ stringify_bool str_state cond
      ^ ") { \n"
      ^ stringify_stmt ~indentation_lvl str_state body
      ^ ind ^ "}\n"
  | Switch_assign (var, exp, cases) ->
      let var_decl =
        ind ^ "uint64_t " ^ var ^ " = 0xFFFF" ^ "FFFF" ^ "FFFF" ^ "FFFF ;\n"
      in
      let switch_start =
        ind ^ "switch (" ^ stringify_bv str_state exp ^ ") {\n"
      in
      let cases =
        List.map
          (fun (bval, enmval) ->
            indent ^ "case " ^ bval ^ ": \n" ^ indent ^ indent ^ var ^ " = "
            ^ enmval ^ " ;\n" ^ indent ^ "break; \n"
          )
          cases
      in
      var_decl ^ switch_start ^ String.concat "" cases ^ " \n" ^ ind ^ "}\n"
  | Switch_assign_struct (typ, var, exp, cases) ->
      let var_decl =
        ind ^ "struct " ^ typ ^ " " ^ var ^ " ;\n" ^ ind ^ "uint8_t " ^ var
        ^ "_is_valid = 0 ;\n"
      in
      let switch_start =
        ind ^ "switch (" ^ stringify_bv str_state exp ^ ") { \n"
      in
      let c_cases =
        cases
        |> List.map (fun (bval, kvs) ->
               let member_assignments =
                 kvs
                 |> List.map (fun (k, v) ->
                        indent ^ var ^ "." ^ k ^ " = "
                        ^ ( match v with
                          | Bool_const c -> if c then "1" else "0"
                          | Bv_const s | Binding s | Enum_lit s -> s
                          )
                        ^ ";"
                    )
               in
               indent ^ "case " ^ bval ^ ": \n"
               ^ String.concat "\n" member_assignments
               ^ "\n" ^ indent ^ var ^ "_is_valid = 1 ; \n" ^ indent
               ^ "break; \n"
           )
      in
      var_decl ^ switch_start ^ String.concat "\n" c_cases ^ ind ^ "}\n"
  | Set_ast_case case ->
      str_state.current_case_name <- String.lowercase_ascii case;
      str_state.current_case_members <-
        Hashtbl.find str_state.constructor_names_to_member_names
          str_state.current_case_name;
      ind ^ ast_c_parameter ^ "->"
      ^ (ast_sail_def_name ^ generated_ast_enum_suffix)
      ^ " = " ^ case ^ " ;\n"
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
        | Exp bv_expr -> stringify_bv str_state bv_expr
      in
      let member_access =
        if
          List.exists
            (fun c -> c = str_state.current_case_name)
            str_state.builtin_cases
        then
           ""
        else "." ^ member_c_name
      in
      ind ^ ast_c_parameter ^ "->"
      ^ (ast_sail_def_name ^ generated_ast_payload_suffix)
      ^ "." ^ str_state.current_case_name ^ member_access ^ " = " ^ rhs_string
      ^ ";\n"
  | Ret_ast -> ind ^ "return " ^ ";\n"
  | Block stmts ->
      String.concat ""
        (List.map (stringify_stmt ~indentation_lvl str_state) stmts)

let stringify_decode_procedure (Proc stmt) case_names_to_members builtin_members
    =
  let procedure_start =
    "void decode(struct " ^ ast_sail_def_name ^ " *" ^ ast_c_parameter
    ^ ", uint64_t " ^ binary_stream_c_parameter ^ ") {\n"
  in
  let procedure_end = "}" in
  let initial_state =
    {
      current_case_name = "";
      current_case_members = [];
      constructor_names_to_member_names = case_names_to_members;
      currently_defined_bv_sizes = Hashtbl.create 100;
      builtin_cases = builtin_members;
    }
  in
  let procedure_body = stringify_stmt initial_state stmt in
  procedure_start ^ procedure_body ^ procedure_end
