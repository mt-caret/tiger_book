open Core_kernel

type unique = unit ref [@@deriving sexp]

type t =
  | INT
  | STRING
  | RECORD of (Symbol.t * t) list * unique
  | ARRAY of t * unique
  | NIL
  | UNIT
  | NAME of Symbol.t * t option ref
[@@deriving sexp]

let ref_to_string (r : unique) = sprintf "%d" (2 * Obj.magic r)

let rec to_string = function
  | INT -> "INT"
  | STRING -> "STRING"
  | RECORD (sl, r) ->
    let fields =
      List.map sl ~f:(fun (field, t) -> field ^ ": " ^ to_string t)
      |> String.concat ~sep:", "
    in
    sprintf "%s: { %s }" (ref_to_string r) fields
  | ARRAY (t, r) -> sprintf !"array (%s) of %s" (ref_to_string r) (to_string t)
  | NIL -> "NIL"
  | UNIT -> "UNIT"
  | NAME (sym, t_opt_ref) ->
    Option.map ~f:to_string !t_opt_ref
    |> Option.value ~default:""
    |> sprintf "%s (%s)" sym
;;

let equal t1 t2 =
  match t1, t2 with
  | INT, INT | STRING, STRING | NIL, NIL | UNIT, UNIT -> true
  | RECORD (_, r1), RECORD (_, r2) -> Ref.compare Unit.compare r1 r2 = 0
  | NAME _, _ | _, NAME _ -> failwith "equality defined only for actual types"
  | _ -> false
;;
