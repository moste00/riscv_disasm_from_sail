open Libsail
open Type_check
open Ast_defs

open Decoder
open Decode_procedure

val gen_decoder : tannot ast -> decoder

val gen_decode_proc : decoder -> decproc
