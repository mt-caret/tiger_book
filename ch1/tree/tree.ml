open Core_kernel

type key = string [@@deriving sexp]

let compare key1 key2 =
  match String.compare key1 key2 with
  | res when res > 0 -> `Gt
  | 0 -> `Eq
  | _ -> `Lt
;;

type 'a tree =
  | Leaf
  | Tree of 'a tree * key * 'a * 'a tree
[@@deriving sexp]

let empty = Leaf

let rec insert (node, key, value) =
  match node with
  | Leaf -> Tree (Leaf, key, value, Leaf)
  | Tree (l, k, v, r) ->
    match compare key k with
    | `Gt -> Tree (l, k, v, insert (r, key, value))
    | `Eq -> Tree (l, k, value, r)
    | `Lt -> Tree (insert (l, key, value), k, v, r)
;;

let rec member (key, node) =
  match node with
  | Leaf -> false
  | Tree (l, k, _, r) ->
    match compare key k with
    | `Gt -> member (key, r)
    | `Eq -> true
    | `Lt -> member (key, l)
;;

let rec lookup (node, key) =
  match node with
  | Leaf -> failwith "key not found"
  | Tree (l, k, v, r) ->
    match compare key k with
    | `Gt -> lookup (r, key)
    | `Eq -> v
    | `Lt -> lookup (l, key)
;;

let insert_list keys =
  List.fold keys ~init:empty ~f:(fun tree key -> insert (tree, key, 0))
;;

let run str =
  String.split str ~on:' '
  |> insert_list
  |> [%sexp_of: int tree]
  |> print_s

let () =
  run "t s p i p f b s t";
  run "a b c d e f g h i"
;;
