open Libsail
open Type_check
open Ast
open Ast_defs
open Decoder

val gen_decoder : string -> tannot ast -> decoder
