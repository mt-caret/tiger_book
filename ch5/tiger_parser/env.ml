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
  Type.
    [ "print", [STRING], UNIT
    ; "flush", [], UNIT
    ; "getchar", [], STRING
    ; "ord", [STRING], INT
    ; "chr", [INT], STRING
    ; "size", [STRING], INT
    ; "substring", [STRING; INT; INT], STRING
    ; "concat", [STRING; STRING], STRING
    ; "not", [INT], INT
    ; "exit", [INT], UNIT ]
  |> List.map ~f:(fun (name, formals, result) -> name, Entry.FunEntry {formals; result})
  |> Symbol.Map.of_alist_exn
;;
