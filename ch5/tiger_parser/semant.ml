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

let add_entry_to_venv venv symbol (entry : Env.Entry.t) =
  Symbol.Map.set venv ~key:symbol ~data:entry
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
    | Some (Env.Entry.VarEntry {ty}) ->
      let%map actual_ty = Type.skip_names pos ty in
      {exp = (); ty = actual_ty; pos})
  | FieldVar (record_var, field, pos) ->
    let%bind {exp = _; ty; pos = _} = transVar venv tenv record_var in
    (match ty with
    | RECORD (fields, _) as record_ty ->
      (match List.Assoc.find fields field ~equal:Symbol.equal with
      | None ->
        sprintf "field %s not found in record type %s" field (Type.to_string record_ty)
        |> Util.or_error_of_string pos
      | Some field_ty ->
        let%map actual_ty = Type.skip_names pos field_ty in
        {exp = (); ty = actual_ty; pos})
    | _ ->
      sprintf "expected record but found %s" (Type.to_string ty)
      |> Util.or_error_of_string pos)
  | SubscriptVar (array_var, e, pos) ->
    let%bind {exp = _; ty; pos = _} = transVar venv tenv array_var
    and () = check_int venv tenv e in
    (match ty with
    | ARRAY (element_ty, _) ->
      let%map actual_ty = Type.skip_names pos element_ty in
      {exp = (); ty = actual_ty; pos}
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
  | OpExp {left; oper; right; pos} ->
    let%bind {exp = _; ty = tyleft; pos = posleft} = transExp venv tenv left
    and {exp = _; ty = tyright; pos = posright} = transExp venv tenv right in
    let%map () =
      match oper with
      | PlusOp | MinusOp | TimesOp | DivideOp ->
        let%bind () = type_check posleft INT tyleft in
        type_check posright INT tyright
      | LtOp | LeOp | GtOp | GeOp ->
        let error t =
          sprintf "expected INT or STRING but found %s" (Type.to_string t)
          |> Util.or_error_of_string pos
        in
        (match Type.is_decided tyleft, Type.is_decided tyright with
        | true, _ ->
          (match%bind Type.skip_names posleft tyleft with
          | Type.INT -> type_check posright INT tyright
          | Type.STRING -> type_check posright STRING tyright
          | _ -> error tyleft)
        | false, true ->
          (match%bind Type.skip_names posright tyright with
          | Type.INT -> type_check posleft INT tyleft
          | Type.STRING -> type_check posleft STRING tyleft
          | _ -> error tyright)
        | false, false ->
          sprintf
            "ambiguous types %s and %s can be INT or STRING"
            (Type.to_string tyleft)
            (Type.to_string tyright)
          |> Util.or_error_of_string pos)
      | EqOp | NeqOp ->
        let error t =
          sprintf
            "expected INT, STRING, ARRAY, or RECORD but found %s"
            (Type.to_string t)
          |> Util.or_error_of_string pos
        in
        (match Type.is_decided tyleft, Type.is_decided tyright with
        | true, _ ->
          (match%bind Type.skip_names posleft tyleft with
          | Type.INT -> type_check posright INT tyright
          | Type.STRING -> type_check posright STRING tyright
          | Type.ARRAY _ as t -> type_check posright t tyright
          | Type.RECORD _ as t -> type_check posright t tyright
          | _ -> error tyleft)
        | false, true ->
          (match%bind Type.skip_names posright tyright with
          | Type.INT -> type_check posleft INT tyleft
          | Type.STRING -> type_check posleft STRING tyleft
          | Type.ARRAY _ as t -> type_check posleft t tyleft
          | Type.RECORD _ as t -> type_check posleft t tyleft
          | _ -> error tyright)
        | false, false ->
          sprintf
            "ambiguous types %s and %s can be INT, STRING, ARRAY, or RECORD"
            (Type.to_string tyleft)
            (Type.to_string tyright)
          |> Util.or_error_of_string pos)
    in
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
    {exp = (); ty = UNIT; pos}
  | IfExp {test; then_; else_; pos} ->
    let%bind () = check_int venv tenv test
    and {exp = _; ty = then_ty; pos = then_pos} = transExp venv tenv then_ in
    (match else_ with
    | None ->
      let%map () = type_check then_pos UNIT then_ty in
      {exp = (); ty = UNIT; pos}
    | Some e ->
      let%bind {exp = _; ty = else_ty; pos = else_pos} = transExp venv tenv e in
      let%map () = type_check else_pos then_ty else_ty in
      {exp = (); ty = then_ty; pos})
  | WhileExp {test; body; pos} ->
    let%bind () = check_int venv tenv test
    and {exp = _; ty = while_ty; pos = while_pos} = transExp venv tenv body in
    let%map () = type_check while_pos UNIT while_ty in
    {exp = (); ty = UNIT; pos}
  | ForExp {var; escape = _; lo; hi; body; pos} ->
    let%bind () = check_int venv tenv lo
    and () = check_int venv tenv hi in
    let venv_with_var = add_entry_to_venv venv var (VarEntry {ty = INT}) in
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
    let%bind () =
      let dup =
        List.find_a_dup fundecs ~compare:(fun (a : Absyn.fundec) b ->
            Symbol.compare a.fun_name b.fun_name )
      in
      match dup with
      | None -> Ok ()
      | Some {fun_name; fun_pos; _} ->
        sprintf "duplicate definition of function %s" fun_name
        |> Util.or_error_of_string fun_pos
    in
    let%bind venv_with_funcs =
      List.fold_result
        fundecs
        ~init:venv
        ~f:(fun accum_venv {fun_name; params; result; body = _; fun_pos = _} ->
          let%map formals =
            List.map params ~f:(fun {name = _; escape = _; typ; pos} ->
                lookup_actual_type pos tenv typ )
            |> Or_error.all
          and result_type =
            match result with
            | Some (annotation, annotation_pos) ->
              lookup_actual_type annotation_pos tenv annotation
            | None -> Ok Type.UNIT
          in
          let func = Env.Entry.FunEntry {formals; result = result_type} in
          add_entry_to_venv accum_venv fun_name func )
    in
    let%map () =
      List.fold_result
        fundecs
        ~init:()
        ~f:(fun () {fun_name; params; result = _; body; fun_pos = _} ->
          let formals, result_type =
            match Symbol.Map.find_exn venv_with_funcs fun_name with
            | FunEntry {formals; result} -> formals, result
            | VarEntry _ as v ->
              raise_s [%message "internal error" (v : Env.Entry.t) (fun_name : Symbol.t)]
          in
          let venv_with_params =
            List.zip_exn params formals
            |> List.fold
                 ~init:venv_with_funcs
                 ~f:(fun accum_venv ({name; escape = _; typ = _; pos = _}, param_type) ->
                   let var = Env.Entry.VarEntry {ty = param_type} in
                   add_entry_to_venv accum_venv name var )
          in
          let%bind {exp = _; ty; pos} = transExp venv_with_params tenv body in
          type_check pos result_type ty )
    in
    venv_with_funcs, tenv
  | VarDec {name; escape = _; typ; init; pos} ->
    let%bind {exp = _; ty = init_ty; pos = init_pos} = transExp venv tenv init in
    let%bind expected_ty =
      match typ with
      | None when Type.equal init_ty NIL ->
        sprintf "record type annotation required" |> Util.or_error_of_string pos
      | None -> Ok init_ty
      | Some (t, p) -> lookup_actual_type p tenv t
    in
    let%map () = type_check init_pos expected_ty init_ty in
    let venv_with_var = add_entry_to_venv venv name (VarEntry {ty = expected_ty}) in
    venv_with_var, tenv
  | TypeDec tydecs ->
    let%bind () =
      let dup =
        List.find_a_dup tydecs ~compare:(fun (a : Absyn.tydec) b ->
            Symbol.compare a.ty_name b.ty_name )
      in
      match dup with
      | None -> Ok ()
      | Some {ty_name; ty_pos; _} ->
        sprintf "duplicate definition of type %s" ty_name
        |> Util.or_error_of_string ty_pos
    in
    (* TODO: check for mutually recursive types that don't pass through record or array *)
    let tenv_with_types =
      List.fold tydecs ~init:tenv ~f:(fun accum_tenv {ty_name; ty = _; ty_pos = _} ->
          let new_type = Type.NAME (ty_name, ref None) in
          Symbol.Map.set accum_tenv ~key:ty_name ~data:new_type )
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
