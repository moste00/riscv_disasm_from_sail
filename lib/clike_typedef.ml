(*
  Instances of this type describe a C-like "ast" type definition
  The structure doesn't actually need to be an actual hierarchical "ast", 
  That's just the name given to the corresponding structure in Sail
  This type definition will be generated from the "ast" scattered union in Sail
*)

type clike_bitvec =
  | Clike_bit
  | Clike_byte
  | Clike_word
  | Clike_dword
  | Clike_qword
type clike_typedef =
  | Clike_enum of string * string * string list
    (* An enum having a possibly-empty type name, a possibly-empty instance name, and several members *)
  | Clike_struct of
      string * string * clike_typedef list (* A struct having members *)
  | Clike_union of string * string * clike_typedef list
    (* An unsafe "union" or variant type, just like the semantics of C *)
  | Clike_builtin of string * clike_bitvec (* A fixed-width machine type *)
