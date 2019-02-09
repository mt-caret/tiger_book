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
    |> Util.or_error_of_string pos
;;

let lookup_actual_type pos tenv typ =
  match Symbol.Map.find tenv typ with
  | None -> sprintf "type %s not found" typ |> Util.or_error_of_string pos
  | Some ty -> Env.actual_type pos tenv ty
;;

let add_entry_to_venv pos venv symbol (entry : Env.Entry.t) =
  match Symbol.Map.add venv ~key:symbol ~data:entry with
  | `Duplicate ->
    let entry_type = match entry with VarEntry _ -> "var" | FunEntry _ -> "function" in
    sprintf "%s %s already exists" entry_type symbol |> Util.or_error_of_string pos
  | `Ok venv_with_entry -> Ok venv_with_entry
;;

let rec transVar venv tenv (var : Absyn.var) =
  let open Or_error.Let_syntax in
  match var with
  | SimpleVar (s, pos) ->
    (match Symbol.Map.find venv s with
    | None -> sprintf "var %s not found" s |> Util.or_error_of_string pos
    | Some (Env.Entry.FunEntry _) ->
      sprintf "%s not a var" s |> Util.or_error_of_string pos
    | Some (Env.Entry.VarEntry {ty}) -> Ok {exp = (); ty; pos})
  | FieldVar (record_var, field, pos) ->
    let%bind {exp = _; ty; pos = _} = transVar venv tenv record_var in
    (match ty with
    | RECORD (fields, _) as record_ty ->
      (match List.Assoc.find fields field ~equal:Symbol.equal with
      | None ->
        sprintf "field %s not found in record type %s" field (Type.to_string record_ty)
        |> Util.or_error_of_string pos
      | Some field_ty -> Ok {exp = (); ty = field_ty; pos})
    | _ ->
      sprintf "expected record but found %s" (Type.to_string ty)
      |> Util.or_error_of_string pos)
  | SubscriptVar (array_var, e, pos) ->
    let%bind {exp = _; ty; pos = _} = transVar venv tenv array_var
    and () = check_int venv tenv e in
    (match ty with
    | ARRAY (element_ty, _) -> Ok {exp = (); ty = element_ty; pos}
    | _ ->
      sprintf "expected array but found %s" (Type.to_string ty)
      |> Util.or_error_of_string pos)

and check_int venv tenv e =
  let open Or_error.Let_syntax in
  let%bind {exp = _; ty; pos} = transExp venv tenv e in
  type_check pos INT ty

and transExp venv tenv (exp : Absyn.exp) =
  let open Or_error.Let_syntax in
  match exp with
  | VarExp v -> transVar venv tenv v
  | NilExp pos -> Ok {exp = (); ty = NIL; pos}
  | IntExp (_, pos) -> Ok {exp = (); ty = INT; pos}
  | StringExp (_, pos) -> Ok {exp = (); ty = STRING; pos}
  | CallExp {func; args; pos} ->
    (match Symbol.Map.find venv func with
    | None -> sprintf "function %s not found" func |> Util.or_error_of_string pos
    | Some (Env.Entry.VarEntry _) ->
      sprintf "%s not a function" func |> Util.or_error_of_string pos
    | Some (Env.Entry.FunEntry {formals; result}) ->
      let args_length = List.length args in
      let formals_length = List.length formals in
      if args_length <> formals_length
      then
        sprintf "%s expects %d arguments but found %d" func formals_length args_length
        |> Util.or_error_of_string pos
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
    | _, _ -> Util.or_error_of_string pos "integer required")
  | RecordExp {fields; typ; pos} ->
    let%bind ty = lookup_actual_type pos tenv typ in
    (match ty with
    | RECORD (expected_fields, _) as actual_ty ->
      let%map () =
        List.fold_result fields ~init:() ~f:(fun () (field, e, pos) ->
            let%bind {exp = _; ty; pos = _} = transExp venv tenv e in
            match List.Assoc.find ~equal:Symbol.equal expected_fields field with
            | None -> sprintf "unexpected field %s" field |> Util.or_error_of_string pos
            | Some expected_type -> type_check pos expected_type ty )
      and () =
        List.fold_result expected_fields ~init:() ~f:(fun () (expected_field, _) ->
            let match_field (field, _, _) = String.equal field expected_field in
            if List.exists fields ~f:match_field
            then Ok ()
            else
              sprintf "field %s not found" expected_field |> Util.or_error_of_string pos
        )
      in
      {exp = (); ty = actual_ty; pos}
    | _ ->
      Type.to_string ty
      |> sprintf "expected record but found %s"
      |> Util.or_error_of_string pos)
  | SeqExp (exps, pos) ->
    let no_value = {exp = (); ty = UNIT; pos} in
    List.fold_result exps ~init:no_value ~f:(fun _ (e, _) -> transExp venv tenv e)
  | AssignExp {var; exp; pos} ->
    let%bind {exp = _; ty = expected_type; pos = _} = transVar venv tenv var
    and {exp = _; ty; pos = _} = transExp venv tenv exp in
    let%map () = type_check pos expected_type ty in
    {exp = (); ty; pos}
  | IfExp {test; then_; else_; pos} ->
    let%bind () = check_int venv tenv test
    and {exp = _; ty = then_ty; pos = _} = transExp venv tenv then_ in
    (match else_ with
    | None -> Ok {exp = (); ty = then_ty; pos}
    | Some e ->
      let%bind {exp = _; ty = else_ty; pos = else_pos} = transExp venv tenv e in
      let%map () = type_check else_pos then_ty else_ty in
      {exp = (); ty = then_ty; pos})
  | WhileExp {test; body; pos} ->
    let%map () = check_int venv tenv test
    and {exp = _; ty = _; pos = _} = transExp venv tenv body in
    {exp = (); ty = UNIT; pos}
  | ForExp {var; escape = _; lo; hi; body; pos} ->
    (* TODO: deal with escape? *)
    let%bind () = check_int venv tenv lo
    and () = check_int venv tenv hi
    and venv_with_var = add_entry_to_venv pos venv var (VarEntry {ty = INT}) in
    let%map {exp = _; ty = _; pos = _} = transExp venv_with_var tenv body in
    {exp = (); ty = UNIT; pos}
  | BreakExp pos -> Ok {exp = (); ty = UNIT; pos}
  | LetExp {decs; body; pos = _} ->
    let%bind venv', tenv' =
      List.fold_result decs ~init:(venv, tenv) ~f:(fun (accum_venv, accum_tenv) dec ->
          transDec accum_venv accum_tenv dec )
    in
    transExp venv' tenv' body
  | ArrayExp {typ; size; init; pos} ->
    let%bind () = check_int venv tenv size
    and {exp = _; ty = init_ty; pos = init_pos} = transExp venv tenv init in
    (match%bind lookup_actual_type pos tenv typ with
    | ARRAY (array_ty, _) as result_ty ->
      let%map () = type_check init_pos array_ty init_ty in
      {exp = (); ty = result_ty; pos}
    | t ->
      sprintf "expected array but found %s" (Type.to_string t)
      |> Util.or_error_of_string pos)

and transDec venv tenv (dec : Absyn.dec) =
  let open Or_error.Let_syntax in
  match dec with
  | VarDec {name; escape = _; typ; init; pos} ->
    (* TODO deal with escape *)
    let%bind {exp = _; ty = init_ty; pos = init_pos} = transExp venv tenv init in
    let%bind expected_ty =
      Option.map ~f:(fun (t, p) -> lookup_actual_type p tenv t) typ
      |> Option.value ~default:(Ok init_ty)
    in
    let%map () = type_check init_pos expected_ty init_ty
    and venv_with_var = add_entry_to_venv pos venv name (VarEntry {ty = init_ty}) in
    venv_with_var, tenv
  | _ -> failwith "TODO"

and transTy _tenv _ty = failwith "TODO"
