open Core_kernel

module Translate = struct
  type exp = unit [@@deriving sexp]
end

type expty =
  { exp : Translate.exp
  ; ty : Type.t
  ; pos : Lexing.position sexp_opaque }
[@@deriving sexp]

let type_check pos expected_type (ty : Type.t) =
  match ty with
  | NAME (_, t_opt_ref) when Option.is_none !t_opt_ref ->
    t_opt_ref := Some expected_type;
    Ok ()
  | _ when Type.equal expected_type ty -> Ok ()
  | _ ->
    sprintf
      "expected type %s but found %s"
      (Type.to_string expected_type)
      (Type.to_string ty)
    |> Util.or_error_of_string pos
;;

let lookup_type pos tenv typ =
  match Symbol.Map.find tenv typ with
  | None -> sprintf "type %s not found" typ |> Util.or_error_of_string pos
  | Some ty -> Ok ty
;;

let lookup_actual_type pos tenv typ =
  let open Or_error.Let_syntax in
  let%bind ty = lookup_type pos tenv typ in
  Type.skip_names pos ty
;;

let add_entry_to_venv pos venv symbol (entry : Env.Entry.t) =
  match Symbol.Map.add venv ~key:symbol ~data:entry with
  | `Duplicate ->
    let entry_type = match entry with VarEntry _ -> "var" | FunEntry _ -> "function" in
    sprintf "%s %s already exists" entry_type symbol |> Util.or_error_of_string pos
  | `Ok venv_with_entry -> Ok venv_with_entry
;;

let transTy tenv (typ : Absyn.ty) =
  (* here we use lookup_type instead of lookup_actual_type so we do not force
     references to undefined types in NAMEs
   *)
  let open Or_error.Let_syntax in
  match typ with
  | NameTy (ty_name, pos) ->
    let%map ty = lookup_type pos tenv ty_name in
    {exp = (); ty; pos}
  | RecordTy (fields, pos) ->
    let%map field_list =
      List.map fields ~f:(fun {name; escape = _; typ; pos = field_pos} ->
          let%map field_ty = lookup_type field_pos tenv typ in
          name, field_ty )
      |> Or_error.all
    in
    {exp = (); ty = RECORD (field_list, Type.Unique.create ()); pos}
  | ArrayTy (ty_name, pos) ->
    let%map element_ty = lookup_type pos tenv ty_name in
    {exp = (); ty = ARRAY (element_ty, Type.Unique.create ()); pos}
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
                 and actual_expected_type = Type.skip_names pos expected_type in
                 type_check pos actual_expected_type ty )
        in
        {exp = (); ty = result; pos})
  | OpExp {left; oper = _; right; pos} ->
    let%bind {exp = _; ty = tyleft; pos = posleft} = transExp venv tenv left
    and {exp = _; ty = tyright; pos = posright} = transExp venv tenv right in
    let%map () = type_check posleft INT tyleft
    and () = type_check posright INT tyright in
    {exp = (); ty = INT; pos}
  | RecordExp {fields; typ; pos} ->
    (match%bind lookup_actual_type pos tenv typ with
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
    | ty ->
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
  | FunctionDec fundecs ->
    let%bind venv_with_funcs =
      List.fold_result
        fundecs
        ~init:venv
        ~f:(fun accum_venv {fun_name; params; result = _; body = _; fun_pos} ->
          let%bind formals =
            List.map params ~f:(fun {name = _; escape = _; typ; pos} ->
                lookup_actual_type pos tenv typ )
            |> Or_error.all
          in
          let func =
            Env.Entry.FunEntry {formals; result = NAME ("return_type", ref None)}
          in
          add_entry_to_venv fun_pos accum_venv fun_name func )
    in
    let%map () =
      List.fold_result
        fundecs
        ~init:()
        ~f:(fun () {fun_name; params; result = optional_annotation; body; fun_pos} ->
          let formals, result_type =
            match Symbol.Map.find_exn venv_with_funcs fun_name with
            | FunEntry {formals; result} -> formals, result
            | VarEntry _ as v ->
              raise_s [%message "internal error" (v : Env.Entry.t) (fun_name : Symbol.t)]
          in
          let%bind venv_with_params =
            List.zip_exn params formals
            |> List.fold_result
                 ~init:venv_with_funcs
                 ~f:(fun accum_venv
                    ({name; escape = _; typ = _; pos = param_pos}, param_type)
                    ->
                   let var = Env.Entry.VarEntry {ty = param_type} in
                   match Symbol.Map.add accum_venv ~key:name ~data:var with
                   | `Ok new_venv -> Ok new_venv
                   | `Duplicate ->
                     sprintf "var %s already exists" name
                     |> Util.or_error_of_string param_pos )
          in
          let%bind {exp = _; ty; pos} = transExp venv_with_params tenv body
          and () =
            match optional_annotation with
            | Some (annotation, annotation_pos) ->
              let%bind annotation_type =
                lookup_actual_type annotation_pos tenv annotation
              in
              type_check fun_pos annotation_type result_type
            | None -> Ok ()
          in
          type_check pos ty result_type )
    in
    venv_with_funcs, tenv
  | VarDec {name; escape = _; typ; init; pos} ->
    let%bind {exp = _; ty = init_ty; pos = init_pos} = transExp venv tenv init in
    let%bind expected_ty =
      Option.map ~f:(fun (t, p) -> lookup_actual_type p tenv t) typ
      |> Option.value ~default:(Ok init_ty)
    in
    let%map () = type_check init_pos expected_ty init_ty
    and venv_with_var = add_entry_to_venv pos venv name (VarEntry {ty = init_ty}) in
    venv_with_var, tenv
  | TypeDec tydecs ->
    let%bind tenv_with_types =
      List.fold_result tydecs ~init:tenv ~f:(fun accum_tenv {ty_name; ty = _; ty_pos} ->
          let new_type = Type.NAME (ty_name, ref None) in
          match Symbol.Map.add accum_tenv ~key:ty_name ~data:new_type with
          | `Ok new_tenv -> Ok new_tenv
          | `Duplicate ->
            sprintf "type %s already exists" ty_name |> Util.or_error_of_string ty_pos )
    in
    let%map () =
      List.fold_result tydecs ~init:() ~f:(fun () {ty_name; ty; ty_pos} ->
          let%bind {exp = _; ty = rhs_ty; pos = _} = transTy tenv_with_types ty in
          let named_type = Symbol.Map.find_exn tenv_with_types ty_name in
          type_check ty_pos rhs_ty named_type )
    in
    venv, tenv_with_types
;;

let transProg (exp : Absyn.exp) =
  let open Or_error.Let_syntax in
  let%map {exp = _; ty = _; pos = _} = transExp Env.base_venv Env.base_tenv exp in
  ()
;;
