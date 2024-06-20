open Libsail
open Type_check
open Ast
open Ast_defs
open Clike_typedef

val gen_def : tannot ast -> clike_typedef
val name_nameless_defs : tannot ast -> clike_typedef -> clike_typedef
