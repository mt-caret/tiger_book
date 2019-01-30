module Type_id : sig
  type t = string [@@deriving sexp, compare]
end

module Id : sig
  type t = string [@@deriving sexp, compare]
end

module Type_fields : sig
  type t = (Id.t * Type_id.t) list [@@deriving sexp, compare]
end

module Type : sig
  type t =
    | Id of Type_id.t
    | Record of Type_fields.t
    | Array of Type_id.t
  [@@deriving sexp, compare]
end

type exp =
  | L_value of l_value
  | No_value
  | Nil
  | Sequence of exp list
  | Integer_literal of int
  | String_literal of string
  | Negation of exp
  | Function_call of (Id.t * exp list)
  | Addition of (exp * exp)
  | Subtraction of (exp * exp)
  | Multiplication of (exp * exp)
  | Division of (exp * exp)
  | Ge of (exp * exp)
  | Gt of (exp * exp)
  | Le of (exp * exp)
  | Lt of (exp * exp)
  | Eq of (exp * exp)
  | Neq of (exp * exp)
  | Or of (exp * exp)
  | And of (exp * exp)
  | Record of (Type_id.t * (Id.t * exp) list)
  | Array of (Type_id.t * exp * exp)
  | Assignment of (l_value * exp)
  | If_then_else of (exp * exp * exp)
  | If_then of (exp * exp)
  | While of (exp * exp)
  | For of (Id.t * exp * exp * exp)
  | Break
  | Let of (dec list * exp list)
  | Parentheses of exp
[@@deriving sexp, compare]
and type_dec =
  { name: Type_id.t
  ; type_: Type.t }
[@@deriving sexp, compare]
and var_dec =
  { var_name: Id.t
  ; var_type_annotation: Type_id.t option
  ; var_content: exp
  }
[@@deriving sexp, compare]
and func_dec =
  { func_name: Id.t
  ; arguments: Type_fields.t
  ; func_type_annotation: Type_id.t option
  ; func_content: exp
  }
[@@deriving sexp, compare]
and dec =
  | Type_dec of type_dec
  | Var_dec of var_dec
  | Func_dec of func_dec
[@@deriving sexp, compare]
and l_value =
  | Id of Id.t
  | Record_access of (l_value * Id.t)
  | Array_access of (l_value * exp)
[@@deriving sexp, compare]
