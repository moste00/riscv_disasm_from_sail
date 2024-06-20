open Libsail
open Ast_defs
open Sail_ast_processor

val foreach_node : 'a ast -> 'a ast_node_processor -> unit
