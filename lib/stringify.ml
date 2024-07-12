open Clike_typedef
open Decode_procedure

let mk_indentation lvl =
  if lvl = 0 then ""
  else (
    let buff = Buffer.create (lvl * 2) in
    for _ = 1 to lvl do
      Buffer.add_string buff "  " (* 2 spaces *)
    done;
    Buffer.contents buff
  )

let stringify_clike_builin builtin =
  "uint"
  ^ ( match builtin with
    | Clike_bit | Clike_byte -> "8"
    | Clike_word -> "16"
    | Clike_dword -> "32"
    | Clike_qword -> "64"
    )
  ^ "_t "

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
  | Clike_builtin (name, bitvec) -> stringify_clike_builin bitvec ^ name ^ " ;"


(* let stringify_decode_procedure (Proc stmt) clike_typedef = 
  stringfy_stmt stmt  *)
 