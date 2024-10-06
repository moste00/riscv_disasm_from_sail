type value =
  | Bv_const of string
  | Bool_const of bool
  | Binding of string
  | Enum_lit of string

type kv_pairs = (string * value) list

type bv2enum_table = (string, string) Hashtbl.t

type bv2struct_table = (string, kv_pairs) Hashtbl.t

type bv2str_table = (string, string) Hashtbl.t

type enum2str_table = (string, string) Hashtbl.t

(* false value then true *)
type bool2str_table = string * string

type struct2str_table = (kv_pairs, string) Hashtbl.t
