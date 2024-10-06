open Libsail
open Ast_defs
open Ast
open Type_check

open Sail_values

type sail_analysis_result

val analyze : (tannot, env) ast -> Env.t -> sail_analysis_result

val get_bv2enum_mapping : sail_analysis_result -> string -> bv2enum_table option

val get_bv2struct_mapping :
  sail_analysis_result -> string -> (string * bv2struct_table) option

val get_size_of_bv_synonym : sail_analysis_result -> string -> int option

val get_case_arg_size : sail_analysis_result -> string -> int -> int option

val is_member_of_enum : sail_analysis_result -> id -> bool

val get_bv2str_mapping : sail_analysis_result -> string -> bv2str_table option

val get_enum2str_mapping :
  sail_analysis_result -> string -> enum2str_table option

val get_bool2str_mapping :
  sail_analysis_result -> string -> bool2str_table option

val get_struct2str_mapping :
  sail_analysis_result -> string -> struct2str_table option
