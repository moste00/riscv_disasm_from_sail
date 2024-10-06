open Libsail
open Ast

type ('a, 'state) ast_node_processor = {
  process_typedef : 'state -> type_def_aux -> id -> unit;
  process_abbrev : 'state -> type_def_aux -> id -> typquant -> typ_arg -> unit;
  process_record :
    'state -> type_def_aux -> id -> typquant -> (typ * id) list -> bool -> unit;
  process_union :
    'state -> type_def_aux -> id -> typquant -> type_union list -> bool -> unit;
  process_union_clause : 'state -> type_union_aux -> id -> id -> typ -> unit;
  process_enum : 'state -> type_def_aux -> id -> id list -> bool -> unit;
  process_bitfield :
    'state -> type_def_aux -> id -> typ -> (id * index_range) list -> unit;
  process_function_clause :
    'state -> 'a funcl_aux -> id -> 'a pexp_funcl -> unit;
  process_mapping :
    'state -> 'a mapdef_aux -> id -> tannot_opt -> 'a mapcl list -> unit;
  process_mapping_bidir_clause :
    'state -> 'a mapcl_aux -> id -> tannot_opt -> 'a mpexp -> 'a mpexp -> unit;
  process_val : 'state -> val_spec_aux -> id -> typquant -> typ -> unit;
}

let default_processor =
  {
    process_typedef = (fun _ _ _ -> ());
    process_abbrev = (fun _ _ _ _ _ -> ());
    process_record = (fun _ _ _ _ _ _ -> ());
    process_union = (fun _ _ _ _ _ _ -> ());
    process_union_clause = (fun _ _ _ _ _ -> ());
    process_enum = (fun _ _ _ _ _ -> ());
    process_bitfield = (fun _ _ _ _ _ -> ());
    process_function_clause = (fun _ _ _ _ -> ());
    process_mapping = (fun _ _ _ _ _ -> ());
    process_mapping_bidir_clause = (fun _ _ _ _ _ _ -> ());
    process_val = (fun _ _ _ _ _ -> ());
  }
