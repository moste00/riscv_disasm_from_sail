open Libsail
open Type_check
open Ast_defs

open Clike_typedef

type typedef_walker

val set_walker_case : typedef_walker -> string -> string

val walk : typedef_walker -> string option

val gen_def : tannot ast -> clike_typedef * typedef_walker
