open Core_kernel

type id = string [@@deriving sexp]

type binop =
  | Plus
  | Minus
  | Times
  | Div
[@@deriving sexp]

type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list
and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp
[@@deriving sexp]

let rec maxargs =
  let rec maxargs_exp =
    function
    | IdExp _
    | NumExp _ -> 0
    | OpExp (exp1, _, exp2) ->
      Int.max (maxargs_exp exp1) (maxargs_exp exp2)
    | EseqExp (stm, exp) ->
      Int.max (maxargs stm) (maxargs_exp exp)
  in
  function
  | CompoundStm (stm1, stm2) ->
    Int.max (maxargs stm1) (maxargs stm2)
  | AssignStm (_, exp) ->
    maxargs_exp exp
  | PrintStm exps ->
    List.map ~f:maxargs_exp exps
    |> List.fold ~init:(List.length exps) ~f:Int.max
;;

type table = (id * int) list [@@deriving sexp]

let emptyEnv = []

let update = List.Assoc.add ~equal:String.equal

let lookup = List.Assoc.find_exn ~equal:String.equal

let interpBinop =
  function
  | Plus -> (+)
  | Minus -> (-)
  | Times -> ( * )
  | Div -> (/)
;;

let rec interpStm (stm, env) =
  match stm with
  | CompoundStm (stm1, stm2) ->
    let env' = interpStm (stm1, env) in
    interpStm (stm2, env')
  | AssignStm (id, exp) ->
    let (value, env') = interpExp (exp, env) in
    update env' id value
  | PrintStm exps ->
    List.fold exps ~init:env ~f:(fun env exp ->
      let (value, env') = interpExp (exp, env) in
      printf "%d\n" value;
      env')
and interpExp (exp, env) =
  match exp with
  | IdExp id ->
    (lookup env id, env)
  | NumExp num ->
    (num, env)
  | OpExp (exp1, binop, exp2) ->
    let (val1, env') = interpExp (exp1, env) in
    let (val2, env'') = interpExp (exp2, env') in
    ((interpBinop binop) val1 val2, env'')
  | EseqExp (stm, exp) ->
    let env' = interpStm (stm, env) in
    interpExp (exp, env')
;;

let interp stm = interpStm (stm, emptyEnv)

let prog =
  CompoundStm
    ( AssignStm ( "a", OpExp (NumExp 5, Plus, NumExp 3))
    , CompoundStm
      ( AssignStm
        ( "b"
        , EseqExp
          (PrintStm [ IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1) ]
          , OpExp (NumExp 10, Times, IdExp "a")
          )
        )
      , PrintStm [ IdExp "b" ]
      )
    )
  
let () =
  [%message "result env" (interp prog : table)]
  |> print_s
