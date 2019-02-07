open Core_kernel

module Entry : sig
  type t =
    | VarEntry of {ty : Type.t}
    | FunEntry of {formals : Type.t list; result : Type.t}
  [@@deriving sexp]
end

type tenv = Type.t Symbol.Map.t [@@deriving sexp]
type venv = Entry.t Symbol.Map.t [@@deriving sexp]

val actual_type : Lexing.position -> tenv -> Type.t -> Type.t Or_error.t
val base_tenv : tenv
val base_venv : venv
