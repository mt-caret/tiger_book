open Core_kernel

module Translate = struct
  type exp = unit [@@deriving sexp]
end

type expty =
  { exp : Translate.exp
  ; ty : Type.t
  ; pos : Lexing.position sexp_opaque }
[@@deriving sexp]

let type_check pos expected_type ty =
  if Type.equal expected_type ty
  then Ok ()
  else
    sprintf
      "expected type %s but found %s"
      (Type.to_string expected_type)
      (Type.to_string ty)
    |> Util.error_of_string pos
    |> Result.fail
;;

let rec transVar _venv _tenv _var = failwith "TODO"

and transExp venv tenv (exp : Absyn.exp) =
  let open Or_error.Let_syntax in
  match exp with
  | VarExp v -> transVar venv tenv v
  | NilExp pos -> Ok {exp = (); ty = NIL; pos}
  | IntExp (_, pos) -> Ok {exp = (); ty = INT; pos}
  | StringExp (_, pos) -> Ok {exp = (); ty = STRING; pos}
  | CallExp {func; args; pos} ->
    (match Symbol.Map.find venv func with
    | None ->
      sprintf "function %s not found" func |> Util.error_of_string pos |> Result.fail
    | Some (Env.Entry.VarEntry _) ->
      sprintf "%s not a function" func |> Util.error_of_string pos |> Result.fail
    | Some (Env.Entry.FunEntry {formals; result}) ->
      let args_length = List.length args in
      let formals_length = List.length formals in
      if args_length <> formals_length
      then
        sprintf "%s expects %d arguments but found %d" func formals_length args_length
        |> Util.error_of_string pos
        |> Result.fail
      else
        let%map () =
          List.zip_exn formals args
          |> List.fold_result ~init:() ~f:(fun () (expected_type, e) ->
                 let%bind {exp = _; ty; pos} = transExp venv tenv e
                 and actual_expected_type = Env.actual_type pos tenv expected_type in
                 type_check pos actual_expected_type ty )
        in
        {exp = (); ty = result; pos})
  | OpExp {left; oper = _; right; pos} ->
    let%bind {exp = _; ty = tyleft; pos = _} = transExp venv tenv left
    and {exp = _; ty = tyright; pos = _} = transExp venv tenv right in
    (match tyleft, tyright with
    | INT, INT -> Ok {exp = (); ty = INT; pos}
    | _, _ -> Util.error_of_string pos "integer required" |> Result.fail)
  | RecordExp {fields; typ; pos} ->
    (match Symbol.Map.find tenv typ with
    | None -> sprintf "type %s not found" typ |> Util.error_of_string pos |> Result.fail
    | Some ty ->
      (match%bind Env.actual_type pos tenv ty with
      | RECORD (expected_fields, _) as actual_ty ->
        let%map () =
          List.fold_result fields ~init:() ~f:(fun () (field, e, pos) ->
              let%bind {exp = _; ty; pos = _} = transExp venv tenv e in
              match List.Assoc.find ~equal:Symbol.equal expected_fields field with
              | None ->
                sprintf "unexpected field %s" field
                |> Util.error_of_string pos
                |> Result.fail
              | Some expected_type -> type_check pos expected_type ty )
        and () =
          List.fold_result expected_fields ~init:() ~f:(fun () (expected_field, _) ->
              let match_field (field, _, _) = String.equal field expected_field in
              if List.exists fields ~f:match_field
              then Ok ()
              else
                sprintf "field %s not found" expected_field
                |> Util.error_of_string pos
                |> Result.fail )
        in
        {exp = (); ty = actual_ty; pos}
      | _ ->
        Type.to_string ty
        |> sprintf "expected record but found %s"
        |> Util.error_of_string pos
        |> Result.fail))
  | SeqExp (exps, pos) ->
    let no_value = {exp = (); ty = UNIT; pos} in
    List.fold_result exps ~init:no_value ~f:(fun _ (e, _) -> transExp venv tenv e)
  | AssignExp {var; exp; pos} ->
    let%bind {exp = _; ty = expected_type; pos = _} = transVar venv tenv var
    and {exp = _; ty; pos = _} = transExp venv tenv exp in
    let%map () = type_check pos expected_type ty in
    {exp = (); ty; pos}
  | _ -> failwith "TODO"

and transDec _venv _tenv _dec = failwith "TODO"
and transTy _tenv _ty = failwith "TODO"
