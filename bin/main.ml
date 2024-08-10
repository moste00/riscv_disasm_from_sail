open Libsail
open Type_check
open Ast
open Riscv_disasm_from_sail
open Constants

let write_c_file ?(optional_includes = []) name code =
  let oc = open_out name in
  let mk_include_lines incs =
    String.concat "\n" (List.map (fun i -> "#include " ^ i ^ "\n") incs)
  in
  let include_string = mk_include_lines Constants.includes in
  let optional_includes_string = mk_include_lines optional_includes in
  Printf.fprintf oc "%s" include_string;
  Printf.fprintf oc "%s" optional_includes_string;
  Printf.fprintf oc "%s" "\n\n\n";
  Printf.fprintf oc "%s" code;
  close_out oc

let sailpath = "/home/mostafa/.opam/default/share/sail/"

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

let ast, types, side_effects =
  Frontend.load_files sailpath dummyoptions initial_typeenv filepaths

let ctypedefs, case_names_to_members, builtin_members =
  Gen_clike_typedef.gen_def ast

let ctypedefs_str = Stringify.stringify_typdef ctypedefs

let dec = Gen_decoder.gen_decoder ast

let proc_dec = Gen_decoder.gen_decode_proc dec

let proc_dec_str =
  Stringify.stringify_decode_procedure proc_dec case_names_to_members
    builtin_members

let () = write_c_file Constants.ast_type_filename ctypedefs_str
let () =
  write_c_file Constants.decode_logic_filename proc_dec_str
    ~optional_includes:["\"" ^ Constants.ast_type_filename ^ "\""]
