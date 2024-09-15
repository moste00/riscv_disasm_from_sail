open Assembler
open Sail_analysis

open Libsail
open Type_check
open Ast_defs

val gen_asm : tannot ast -> sail_analysis_result -> assembler
