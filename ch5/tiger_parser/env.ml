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
  let open Or_error.Let_syntax in
  match ty with
  | NAME (sym, t_opt_ref) ->
    (match !t_opt_ref with
    | Some t -> Ok t
    | None ->
      (match Symbol.Map.find tenv sym with
      | Some new_ty ->
        let%map result = actual_type pos tenv new_ty in
        t_opt_ref := Some result;
        result
      | None -> sprintf "%s not found" sym |> Util.error_of_string pos |> Result.fail))
  | t -> Ok t
;;

let base_tenv = Symbol.Map.empty
let base_venv = Symbol.Map.empty
