open Core_kernel

module T = struct
  type t = string [@@deriving sexp, compare]
end

include T
include Comparable.Make (T)
