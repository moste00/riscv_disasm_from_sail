open Libsail
open Ast

type ast_node_processor = {
  process_typedef : type_def_aux -> id -> unit;
  process_abbrev : type_def_aux -> id -> typquant -> typ_arg -> unit;
  process_record :
    type_def_aux -> id -> typquant -> (typ * id) list -> bool -> unit;
  process_union :
    type_def_aux -> id -> typquant -> type_union list -> bool -> unit;
  process_enum : type_def_aux -> id -> id list -> bool -> unit;
  process_bitfield :
    type_def_aux -> id -> typ -> (id * index_range) list -> unit;
}

let default_processor =
  {
    process_typedef = (fun _ _ -> ());
    process_abbrev = (fun _ _ _ _ -> ());
    process_record = (fun _ _ _ _ _ -> ());
    process_union = (fun _ _ _ _ _ -> ());
    process_enum = (fun _ _ _ _ -> ());
    process_bitfield = (fun _ _ _ _ -> ());
  }
