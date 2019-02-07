%token TYPE
%token VAR
%token FUNCTION
%token BREAK
%token OF
%token END
%token IN
%token NIL
%token LET
%token DO
%token TO
%token FOR
%token WHILE
%token ELSE
%token THEN
%token IF
%token ARRAY
%token ASSIGN
%token OR
%token AND
%token GE
%token GT
%token LE
%token LT
%token NEQ
%token EQ
%token DIVIDE
%token TIMES
%token MINUS
%token PLUS
%token DOT
%token RBRACE
%token LBRACE
%token RBRACK
%token LBRACK
%token RPAREN
%token LPAREN
%token SEMICOLON
%token COLON
%token COMMA
%token <string> STRING
%token <int> INT
%token <string> ID
%token EOF

%{
open Core_kernel
open Absyn

let compact_decs decs =
  List.fold
    decs
    ~init:[]
    ~f:(fun accum x ->
        match (x, accum) with
        | (`F f, FunctionDec fs :: rem) -> FunctionDec (f :: fs) :: rem
        | (`F f, _) -> FunctionDec [f] :: accum
        | (`T t, TypeDec ts :: rem) -> TypeDec (t :: ts) :: rem
        | (`T t, _) -> TypeDec [t] :: accum
        | (`V v, _) -> v :: accum)
  |> List.map
      ~f:(function
          | FunctionDec fs -> FunctionDec (List.rev fs)
          | TypeDec ts -> TypeDec (List.rev ts)
          | x -> x)
  |> List.rev
;;
%}

%left OR
%left AND
%nonassoc GE GT LE LT NEQ EQ
%left PLUS MINUS
%left TIMES DIVIDE

%start <Absyn.exp> program
%%

program:
  | e = exp; EOF { e }
  ;

literal:
  | i = INT { IntExp (i, $startpos) }
  | s = STRING { StringExp (s, $startpos) }
  ;

field:
  p = separated_pair(ID, EQ, exp) { (fst p, snd p, $startpos) };

seqexp:
  e = exp { (e, $startpos) };

term:
  | NIL { NilExp $startpos }
  | LPAREN; e = seqexp; SEMICOLON; es = separated_nonempty_list(SEMICOLON, seqexp); RPAREN
    { SeqExp (e :: es, $startpos) }
  | LPAREN; RPAREN { SeqExp ([], $startpos) }
  | l = literal { l }
  | MINUS; right = term
    { OpExp { left = IntExp (0, $startpos); oper = MinusOp; right; pos = $startpos } }
  | func = ID; LPAREN; args = separated_list(COMMA, exp); RPAREN
    { CallExp { func; args; pos = $startpos } }
  | test = term; OR; else_ = term
    { IfExp { test; then_ = IntExp (1, $startpos); else_ = Some else_; pos = $startpos } }
  | test = term; AND; then_ = term
    { IfExp { test; then_; else_ = Some (IntExp (0, $startpos)); pos = $startpos } }
  | left = term; GE; right = term
    { OpExp { left; oper = GeOp; right; pos = $startpos } }
  | left = term; GT; right = term
    { OpExp { left; oper = GtOp; right; pos = $startpos } }
  | left = term; LE; right = term
    { OpExp { left; oper = LeOp; right; pos = $startpos } }
  | left = term; LT; right = term
    { OpExp { left; oper = LtOp; right; pos = $startpos } }
  | left = term; NEQ; right = term
    { OpExp { left; oper = EqOp; right; pos = $startpos } }
  | left = term; EQ; right = term
    { OpExp { left; oper = NeqOp; right; pos = $startpos } }
  | left = term; DIVIDE; right = term
    { OpExp { left; oper = DivideOp; right; pos = $startpos } }
  | left = term; TIMES; right = term
    { OpExp { left; oper = TimesOp; right; pos = $startpos } }
  | left = term; MINUS; right = term
    { OpExp { left; oper = MinusOp; right; pos = $startpos } }
  | left = term; PLUS; right = term
    { OpExp { left; oper = PlusOp; right; pos = $startpos } }
  | typ = ID; LBRACE; fields = separated_list(COMMA, field); RBRACE
    { RecordExp { fields; typ; pos = $startpos } }
  | LPAREN; e = exp; RPAREN { e }
  | l = l_value { VarExp l }
  ;

dangling_else:
  | { None }
  | ELSE; else_ = exp { Some else_ }

exp:
  | t = term { t }
  | IF; test = exp; THEN; then_ = term; else_ = dangling_else
    { IfExp { test; then_; else_; pos = $startpos } }
  | WHILE; test = exp; DO; body = exp
    { WhileExp { test; body; pos = $startpos } }
  | FOR; var = ID; lo = assign; TO; hi = exp; DO; body = exp
    { ForExp { var; escape = ref true; lo; hi; body; pos = $startpos } }
  | BREAK { BreakExp $startpos }
  | typ = ID; LBRACK; size = exp; RBRACK; OF init = exp
    { ArrayExp { typ; size; init; pos = $startpos } }
  | LET; decs = list(dec); IN; exps = separated_list(SEMICOLON, seqexp); END
    { LetExp { decs = compact_decs decs; body = SeqExp (exps, $startpos(exps)); pos = $startpos } }
  | var = l_value; exp = assign
    { AssignExp { var; exp; pos = $startpos } }
  ;

assign:
  ASSIGN; e = exp { e };

l_value:
  i = ID; l = list(l_value_access)
    { List.fold
        l
        ~init:(SimpleVar (i, $startpos))
        ~f:(fun accum ->
             function
             | `Record (i, p) -> FieldVar (accum, i, p)
             | `Array (e, p) -> SubscriptVar (accum, e, p))
    };

l_value_access:
  | DOT; i = ID { `Record (i, $startpos) }
  | LBRACK; e = exp; RBRACK { `Array (e, $startpos) }
  ;

dec:
  | t = tydec { `T t }
  | VAR; name = ID; typ = option(type_annotation); ASSIGN; init = exp
    { `V (VarDec { name; escape = ref true; typ; init; pos = $startpos }) }
  | f = fundec { `F f }
  ;

tydec:
  TYPE; ty_name = ID; EQ; ty = type_ { { ty_name; ty; ty_pos = $startpos } };

type_:
  | LBRACE; t = tyfields; RBRACE { RecordTy t }
  | ARRAY; OF; t = ID { ArrayTy (t, $startpos) }
  | t = ID { NameTy (t, $startpos) }
  ;

tyfield:
  t = separated_pair(ID, COLON, ID)
    { { name = fst t; escape = ref true; typ = snd t; pos = $startpos }}

tyfields:
  t = separated_list(COMMA, tyfield) { t };

type_annotation:
  COLON; type_ = ID; { (type_, $startpos) }

fundec:
  FUNCTION; fun_name = ID; LPAREN; params = tyfields; RPAREN; result = option(type_annotation); EQ; body = exp
    { { fun_name; params; result; body; fun_pos = $startpos } }
