open Core_kernel

type t = [
  | `Assoc of (string * t) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of t list
  | `Null
  | `String of string
  ] [@@deriving sexp]
