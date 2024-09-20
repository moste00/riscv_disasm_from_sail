open Common_types

type case_id = string

type tostr_logic =
  | Lit of string
  | Bitv2Str of string * bv2str_table
  | Enum2Str of string * enum2str_table
  | Bool2Str of string * bool2str_table
  | Struct2str of string * string * struct2str_table
  | Intrinsic_tostr_logic of string * string list

type assembler_clause = case_id * tostr_logic list

type assembler = assembler_clause list
