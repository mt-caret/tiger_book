open Core_kernel

module Unique = struct
  type t = int [@@deriving sexp, compare]

  let create =
    let seed = ref 0 in
    fun () ->
      seed := !seed + 1;
      !seed
  ;;

  let equal = [%compare.equal: t]
  let to_string = sprintf "%d"
end

type t =
  | INT
  | STRING
  | RECORD of (Symbol.t * t) list * Unique.t
  | ARRAY of t * Unique.t
  | NIL
  | UNIT
  | NAME of Symbol.t * t option ref
[@@deriving sexp]

let is_recursive =
  let rec go accum = function
    | INT | STRING | NIL | UNIT -> false
    | NAME (_, t_opt_ref) ->
      (match !t_opt_ref with None -> false | Some t -> go accum t)
    | RECORD (fields, u) ->
      if List.mem accum u ~equal:Unique.equal
      then true
      else List.map ~f:snd fields |> List.exists ~f:(go (u :: accum))
    | ARRAY (t, u) ->
      if List.mem accum u ~equal:Unique.equal then true else go (u :: accum) t
  in
  go []
;;

let rec to_string =
  let to_string_norec t = if is_recursive t then "... (recursive)" else to_string t in
  function
  | INT -> "INT"
  | STRING -> "STRING"
  | RECORD (sl, r) ->
    let fields =
      List.map sl ~f:(fun (field, t) -> field ^ ": " ^ to_string_norec t)
      |> String.concat ~sep:", "
    in
    sprintf "{ %s } (%s)" fields (Unique.to_string r)
  | ARRAY (t, r) -> sprintf !"array (%s) of %s" (Unique.to_string r) (to_string_norec t)
  | NIL -> "NIL"
  | UNIT -> "UNIT"
  | NAME (sym, t_opt_ref) ->
    Option.map ~f:to_string !t_opt_ref
    |> Option.value ~default:"empty"
    |> sprintf "%s (%s)" sym
;;

let rec equal t1 t2 =
  match t1, t2 with
  | INT, INT | STRING, STRING | NIL, NIL | UNIT, UNIT -> true
  | ARRAY (_, u1), ARRAY (_, u2) -> Unique.equal u1 u2
  | RECORD _, NIL -> true
  | RECORD (_, u1), RECORD (_, u2) -> Unique.equal u1 u2
  | NAME (_, t_opt_ref1), NAME (_, t_opt_ref2) when phys_equal t_opt_ref1 t_opt_ref2 ->
    true
  | NAME (_, t_opt_ref), _ ->
    (match !t_opt_ref with
    | Some t -> equal t t2
    | None -> raise_s [%message "type equality error" (t1 : t) (t2 : t)])
  | _, NAME (_, t_opt_ref) ->
    (match !t_opt_ref with
    | Some t -> equal t1 t
    | None -> raise_s [%message "type equality error" (t1 : t) (t2 : t)])
  | _, _ -> false
;;

let rec skip_names (pos : Lexing.position) (ty : t) =
  match ty with
  | NAME (sym, t_opt_ref) ->
    (match !t_opt_ref with
    | Some t -> skip_names pos t
    | None ->
      sprintf "dangling reference from type %s" sym |> Util.or_error_of_string pos)
  | t -> Ok t
;;
