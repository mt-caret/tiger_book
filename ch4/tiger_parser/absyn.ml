open Core_kernel

type pos = Lexing.position sexp_opaque [@@deriving sexp]
type symbol = string [@@deriving sexp, compare]

type field =
  { name : symbol
  ; escape : bool ref
  ; typ : symbol
  ; pos : pos }
[@@deriving sexp]

type var =
  | SimpleVar of symbol * pos
  | FieldVar of var * symbol * pos
  | SubscriptVar of var * exp * pos
[@@deriving sexp]

and exp =
  | VarExp of var
  | NilExp
  | IntExp of int
  | StringExp of string * pos
  | CallExp of {func : symbol; args : exp list; pos : pos}
  | OpExp of {left : exp; oper : oper; right : exp; pos : pos}
  | RecordExp of {fields : (symbol * exp * pos) list; typ : symbol; pos : pos}
  | SeqExp of (exp * pos) list
  | AssignExp of {var : var; exp : exp; pos : pos}
  | IfExp of {test : exp; then_ : exp; else_ : exp option; pos : pos}
  | WhileExp of {test : exp; body : exp; pos : pos}
  | ForExp of {var : symbol; escape : bool ref; lo : exp; hi : exp; body : exp; pos : pos}
  | BreakExp of pos
  | LetExp of {decs : dec list; body : exp; pos : pos}
  | ArrayExp of {typ : symbol; size : exp; init : exp; pos : pos}
[@@deriving sexp]

and dec =
  | FunctionDec of fundec list
  | VarDec of
      { name : symbol
      ; escape : bool ref
      ; typ : (symbol * pos) option
      ; init : exp
      ; pos : pos }
  | TypeDec of tydec list
[@@deriving sexp]

and ty =
  | NameTy of symbol * pos
  | RecordTy of field list
  | ArrayTy of symbol * pos
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
  { fun_name : symbol
  ; params : field list
  ; result : (symbol * pos) option
  ; body : exp
  ; fun_pos : pos }
[@@deriving sexp]

and tydec =
  { ty_name : symbol
  ; ty : ty
  ; ty_pos : pos }
[@@deriving sexp]
