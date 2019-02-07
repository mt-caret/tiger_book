open Core_kernel

type pos = Lexing.position sexp_opaque [@@deriving sexp]

type field =
  { name : Symbol.t
  ; escape : bool ref
  ; typ : Symbol.t
  ; pos : pos }
[@@deriving sexp]

type var =
  | SimpleVar of Symbol.t * pos
  | FieldVar of var * Symbol.t * pos
  | SubscriptVar of var * exp * pos
[@@deriving sexp]

and exp =
  | VarExp of var
  | NilExp of pos
  | IntExp of int * pos
  | StringExp of string * pos
  | CallExp of {func : Symbol.t; args : exp list; pos : pos}
  | OpExp of {left : exp; oper : oper; right : exp; pos : pos}
  | RecordExp of {fields : (Symbol.t * exp * pos) list; typ : Symbol.t; pos : pos}
  | SeqExp of (exp * pos) list * pos
  | AssignExp of {var : var; exp : exp; pos : pos}
  | IfExp of {test : exp; then_ : exp; else_ : exp option; pos : pos}
  | WhileExp of {test : exp; body : exp; pos : pos}
  | ForExp of
      { var : Symbol.t
      ; escape : bool ref
      ; lo : exp
      ; hi : exp
      ; body : exp
      ; pos : pos }
  | BreakExp of pos
  | LetExp of {decs : dec list; body : exp; pos : pos}
  | ArrayExp of {typ : Symbol.t; size : exp; init : exp; pos : pos}
[@@deriving sexp]

and dec =
  | FunctionDec of fundec list
  | VarDec of
      { name : Symbol.t
      ; escape : bool ref
      ; typ : (Symbol.t * pos) option
      ; init : exp
      ; pos : pos }
  | TypeDec of tydec list
[@@deriving sexp]

and ty =
  | NameTy of Symbol.t * pos
  | RecordTy of field list
  | ArrayTy of Symbol.t * pos
[@@deriving sexp]

and oper =
  | PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp
[@@deriving sexp]

and fundec =
  { fun_name : Symbol.t
  ; params : field list
  ; result : (Symbol.t * pos) option
  ; body : exp
  ; fun_pos : pos }
[@@deriving sexp]

and tydec =
  { ty_name : Symbol.t
  ; ty : ty
  ; ty_pos : pos }
[@@deriving sexp]
