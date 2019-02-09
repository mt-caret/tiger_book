open Core_kernel

module Entry = struct
  type t =
    | VarEntry of {ty : Type.t}
    | FunEntry of {formals : Type.t list; result : Type.t}
  [@@deriving sexp]
end

type tenv = Type.t Symbol.Map.t [@@deriving sexp]
type venv = Entry.t Symbol.Map.t [@@deriving sexp]

let rec actual_type (pos : Lexing.position) tenv (ty : Type.t) =
  match ty with
  | NAME (sym, t_opt_ref) ->
    (match !t_opt_ref with
    | Some t -> actual_type pos tenv t
    | None ->
      sprintf "dangling reference from type %s" sym |> Util.or_error_of_string pos)
  | t -> Ok t
;;

let base_tenv = Symbol.Map.of_alist_exn ["int", Type.INT; "string", Type.STRING]

let base_venv =
  (* TODO: add predefined function bindings (p.114) *)
  Symbol.Map.empty
;;
