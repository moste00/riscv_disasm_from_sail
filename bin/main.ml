open Libsail
open Type_check
open Ast
open Riscv_disasm_from_sail
open Constants
open Printexc

let mkdir_if_none_exists dirname =
  try Sys.mkdir dirname 0o777 with Sys_error _ -> ()

let write_c_file ?(additional_includes = []) name code =
  mkdir_if_none_exists "riscv_disasm";

  let oc = open_out ("riscv_disasm/" ^ name) in
  let mk_include_lines incs =
    String.concat "\n" (List.map (fun i -> "#include " ^ i ^ "\n") incs)
  in
  let include_string = mk_include_lines Constants.includes in
  let additional_includes_string = mk_include_lines additional_includes in
  let name_no_dots = String.map (fun c -> if c = '.' then '_' else c) name in
  Printf.fprintf oc "%s"
    ("#ifndef __" ^ String.capitalize_ascii name_no_dots ^ "__\n");
  Printf.fprintf oc "%s"
    ("#define __" ^ String.capitalize_ascii name_no_dots ^ "__\n");
  Printf.fprintf oc "%s" include_string;
  Printf.fprintf oc "%s" additional_includes_string;
  Printf.fprintf oc "%s" "\n\n";
  Printf.fprintf oc "%s" code;
  Printf.fprintf oc "%s" "\n\n #endif\n";
  close_out oc

let sailpath = Unix.getenv "HOME" ^ "/.opam/default/share/sail/"

let paths_filename = ref ""

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

let _, ast, types, side_effects =
  try Frontend.load_files sailpath dummyoptions initial_typeenv filepaths
  with Reporting.Fatal_error e as ex ->
    Reporting.print_error e;
    raise ex

let ctypedefs, typdefwalker = Gen_clike_typedef.gen_def ast

let ctypedefs_str = Stringify.stringify_typdef ctypedefs

let analysis = Sail_analysis.analyze ast types

let dec = Gen_decoder.gen_decoder ast analysis

let proc_dec = Gen_decoder.gen_decode_proc dec

let proc_dec_str = Stringify.stringify_decode_procedure proc_dec typdefwalker

let asm = Gen_assembler.gen_asm ast analysis

let asm_str, tables_str = Stringify.stringify_assembler asm typdefwalker

let () = write_c_file Constants.ast_type_filename ctypedefs_str
let () =
  write_c_file Constants.decode_logic_filename proc_dec_str
    ~additional_includes:["\"" ^ Constants.ast_type_filename ^ "\""]

let () =
  write_c_file Constants.assembler_filename asm_str
    ~additional_includes:
      [
        "\"" ^ Constants.ast_type_filename ^ "\"";
        "\"" ^ Constants.ast2str_tables_filename ^ "\"";
        "\"riscv_helpers_ast2str.h\"";
      ]

let () =
  write_c_file Constants.ast2str_tables_filename tables_str
    ~additional_includes:["\"" ^ Constants.ast_type_filename ^ "\""]
