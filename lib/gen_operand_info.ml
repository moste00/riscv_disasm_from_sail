open Libsail
open Ast

open Sail_ast_foreach
open Sail_ast_processor
open Sail_utils
open Sail_analysis

open Utils

(* The four register files in RISCV and its standard extensions *)
(* The float and double register files overlap, but they should still
   be counted as seperate for max clarity of information provided
   by the generated disassembler *)
type regfile = Base | Float | Double | Vector
type operand = Reg of int * regfile

type operand_info = (string, operand list) Hashtbl.t

type operand_gen_iteration_state = {
  analysis : sail_analysis_result;
  info : operand_info;
}

let destructure_arglist args =
  let (P_aux (args_pat, _)) = args in
  match args_pat with
  | P_app (id, arg_names) -> (
      let args_tuple = Option.get (get_sole_element_or_none arg_names) in
      match args_tuple with
      | P_aux (P_tuple args, _) ->
          print_endline
            "\n--------------------SUCC----------------------------\n";
          ( id_to_str id,
            List.map
              (fun (P_aux (p, _)) ->
                match p with P_id i -> id_to_str i | _ -> failwith "******"
              )
              args
          )
      | _ -> (id_to_str id, [])
    )
  | _ -> failwith "__++__"

let bind_regidx_args reg_indices_members arg_names =
  let binding = Hashtbl.create (List.length reg_indices_members) in
  List.iteri
    (fun i arg_name ->
      if List.mem i reg_indices_members then Hashtbl.add binding arg_name i
    )
    arg_names;
  binding

let analyze_operands state _ fun_id func =
  if id_to_str_noexn fun_id <> "execute" then
    print_endline
      ("\n\n -------------------- ENCOUNTERED " ^ id_to_str_noexn fun_id
     ^ " FUNCTION, IGNORING IT \n\n"
      )
  else (
    let (Pat_aux (pat, _)) = func in
    let args, body =
      match pat with Pat_exp (a, b) -> (a, b) | Pat_when (a, b, _) -> (a, b)
    in
    let case_name, arg_names = destructure_arglist args in
    let reg_indices_members =
      get_all_case_members_of_type_named state.analysis case_name "regidx"
    in
    let bound_args = bind_regidx_args reg_indices_members arg_names in
    print_endline
      ("+++++++++++++++++++++++++++ IN CASE " ^ case_name
     ^ " +++++++++++++++++++++++++++++++"
      );
    Hashtbl.iter
      (fun arg_name arg_idx ->
        print_endline
          ("&&&&&&&&&&&&&&&&&&&&&& ARG " ^ arg_name ^ " IS ARG "
         ^ string_of_int arg_idx ^ " )))))) \n\n"
          )
      )
      bound_args
  )

let gen_operand_info ast analysis =
  let processor =
    { default_processor with process_function_clause = analyze_operands }
  in
  let state = { analysis; info = Hashtbl.create 300 } in
  foreach_node ast processor state;
  print_endline
    "\n\n\
     ================================== OPERAND ANALYSIS DONE \
     =============================================== \n\n\
    \ "
