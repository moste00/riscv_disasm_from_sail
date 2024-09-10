open Assembler

open Libsail
open Type_check
open Ast_defs

val gen_asm : tannot ast -> assembler
