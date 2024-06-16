open Libsail
open Type_check
open Ast
open Ast_defs
open Clike_typedef

val gen_def : string -> tannot ast -> clike_typedef
