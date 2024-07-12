open Decoder

type bv_val = int64
type bv_index = int64
type enum_val = string

type bv_expr =
  | Literal of bv_val
  | Binstr_slice of bv_index * bv_index
  | Concat of bv_expr list

type boolean_expr =
  | Is_eq of bv_expr * bv_val
  | Is_enum_var_valid of string
  | And of boolean_expr list

type stmt =
  | Declare of string
  | Init of string * bv_expr
  | If of boolean_expr * stmt
  | Switch_assign of string * bv_expr * (bv_val * enum_val) list
  | Set_ast_case of string
  | Set_ast_next_case_member of (value option * bv_expr option)
  | Ret_ast
  | Block of stmt list

type decproc = Proc of stmt
