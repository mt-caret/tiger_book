open Core_kernel

module Unique : sig
  type t [@@deriving sexp, compare]

  val create : unit -> t
  val equal : t -> t -> bool
end

type t =
  | INT
  | STRING
  | RECORD of (Symbol.t * t) list * Unique.t
  | ARRAY of t * Unique.t
  | NIL
  | UNIT
  | NAME of Symbol.t * t option ref
[@@deriving sexp]

val to_string : t -> string
val equal : t -> t -> bool
val skip_names : Lexing.position -> t -> t Or_error.t
val is_decided : t -> bool
val has_illegal_cycle : t -> bool
