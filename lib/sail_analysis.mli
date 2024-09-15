open Libsail
open Ast_defs
open Type_check

open Common_types

type sail_analysis_result

val analyze : tannot ast -> sail_analysis_result

val get_bv2enum_mapping : sail_analysis_result -> string -> bv2enum_table option

val get_bv2struct_mapping :
  sail_analysis_result -> string -> (string * bv2struct_table) option

val get_size_of_bv_synonym : sail_analysis_result -> string -> int option

val get_case_arg_size : sail_analysis_result -> string -> int -> int option
