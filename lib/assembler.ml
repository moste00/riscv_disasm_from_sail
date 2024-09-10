open Common_types

type case_id = string

type tostr_logic =
  | Lit of string
  | Bitv2Str of (string, string) Hashtbl.t
  | Enum2Str of (string, string) Hashtbl.t
  | Bool2Str of string * string
  | Struct2str of (string, kv_pairs) Hashtbl.t
  | Intrinsic_tostr_logic of string

type assembler_clause = case_id * tostr_logic list

type assembler = assembler_clause list
