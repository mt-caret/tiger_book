open Core_kernel

type t = string [@@deriving sexp, compare]

include Comparable.S with type t := t
