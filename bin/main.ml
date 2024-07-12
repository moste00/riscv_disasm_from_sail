open Libsail
open Type_check
open Ast
open Riscv_disasm_from_sail

let sailpath = "/home/mostafa/.opam/default/share/sail/"

let paths_filename = ref "test_filepaths.txt"
let usage_msg = "Usage: riscv_disasm_from_sail -f <path-to-list-of-input-files>"
let arg_spec =
  [
    ( "-f",
      Arg.Set_string paths_filename,
      "Path to a file containing a list of input files, a filename on each line"
    );
  ]
let anon_arg_handler a =
  print_endline ("Unrecognized argument " ^ a ^ ", ignoring...")

let () = Arg.parse arg_spec anon_arg_handler usage_msg

let read_filepaths name =
  let names = ref [] in
  let file_chnl = open_in name in
  try
    while true do
      names := input_line file_chnl :: !names
    done;
    [""] (*UNREACHABLE, just to guide type checker*)
  with
  | End_of_file ->
      close_in file_chnl;
      List.rev !names
  | e ->
      close_in_noerr file_chnl;
      raise e

let filepaths = read_filepaths !paths_filename

let initial_typeenv = Type_check.initial_env

let dummyoptions =
  [
    ("-lem_extern_type", Arg.String (fun _ -> ()), "");
    ("-coq_extern_type", Arg.String (fun _ -> ()), "");
  ]

let ast, types, side_effects =
  Frontend.load_files sailpath dummyoptions initial_typeenv filepaths

let ctypedefs = Gen_clike_typedef.gen_def ast

let n_ctypedefs = Gen_clike_typedef.name_nameless_defs ast ctypedefs

let ctypedefs_str = Stringify.stringify_clike_typedef n_ctypedefs

let () = print_endline ctypedefs_str

let dec = Gen_decoder.gen_decoder ast

let __ = Gen_decoder.gen_decode_proc dec