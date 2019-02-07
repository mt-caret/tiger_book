open Core_kernel

module Translate : sig
  type exp = unit [@@deriving sexp]
end

type expty =
  { exp : Translate.exp
  ; ty : Type.t
  ; pos : Lexing.position sexp_opaque }
[@@deriving sexp]

val transVar : Env.venv -> Env.tenv -> Absyn.var -> expty Or_error.t
val transExp : Env.venv -> Env.tenv -> Absyn.exp -> expty Or_error.t
val transDec : Env.venv -> Env.tenv -> Absyn.dec -> (Env.venv * Env.tenv) Or_error.t
val transTy : Env.tenv -> Absyn.ty -> expty Or_error.t
