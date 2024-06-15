open Libsail
open Type_check
open Ast
open Ast_defs
open Clike_ast

type type_registery = type_def_aux Map.Make(String).t

val collect_types : tannot ast -> type_registery
val gen_clike_ast : string -> type_registery -> clike_type
