open Sail_values

type case_name = string

type bv_lit_xor_arg_idx =
  | Bv_lit of string * int (* bitv size of literal *)
  | Arg_idx of int * int (* bitv size of arg *)

type intrinsic_logic_arg =
  | Arg_index of int
  | Bv_concat of bv_lit_xor_arg_idx list

type tostr_logic =
  | Lit of string
  | Bitv2Str of int * bv2str_table
  | Enum2Str of int * enum2str_table
  | Bool2Str of int * bool2str_table
  | Struct2str of int * struct2str_table
  | Intrinsic_tostr_logic of string * intrinsic_logic_arg list

type subcase_condition = (int * string) option

type subcase = subcase_condition * tostr_logic list

type assembler_clause = case_name * subcase list

type assembler = assembler_clause list
