open Libsail
open Type_check
open Ast_defs
open Clike_typedef

val gen_def : tannot ast -> clike_typedef * (string, string list) Hashtbl.t * string list
