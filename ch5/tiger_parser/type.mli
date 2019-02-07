type unique = unit ref [@@deriving sexp]

type t =
  | INT
  | STRING
  | RECORD of (Symbol.t * t) list * unique
  | ARRAY of t * unique
  | NIL
  | UNIT
  | NAME of Symbol.t * t option ref
[@@deriving sexp]

val to_string : t -> string
val equal : t -> t -> bool
