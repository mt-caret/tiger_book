open Core_kernel

module Entry = struct
  type t =
    | VarEntry of {ty : Type.t}
    | FunEntry of {formals : Type.t list; result : Type.t}
  [@@deriving sexp]
end

type tenv = Type.t Symbol.Map.t [@@deriving sexp]
type venv = Entry.t Symbol.Map.t [@@deriving sexp]

let base_tenv = Symbol.Map.of_alist_exn ["int", Type.INT; "string", Type.STRING]

let base_venv =
  (* TODO: add predefined function bindings (p.114) *)
  Symbol.Map.empty
;;
