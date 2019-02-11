open Core_kernel

module Translate : sig
  type exp = unit [@@deriving sexp]
end

type expty =
  { exp : Translate.exp
  ; ty : Type.t
  ; pos : Lexing.position sexp_opaque }
[@@deriving sexp]

val transProg : Absyn.exp -> unit Or_error.t
