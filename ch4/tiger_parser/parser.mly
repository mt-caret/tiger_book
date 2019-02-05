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
open Tiger
%}

%left OR
%left AND
%nonassoc GE GT LE LT NEQ EQ
%left PLUS MINUS
%left TIMES DIVIDE

%start <Tiger.exp> program
%%

program:
  | e = exp; EOF { e }
  ;

literal:
  | i = INT { Integer_literal i }
  | s = STRING { String_literal s }
  ;

term:
  | NIL { Nil }
  | LPAREN; e = exp; SEMICOLON; es = separated_nonempty_list(SEMICOLON, exp); RPAREN
    { Sequence (e :: es) }
  | l = literal { l }
  | MINUS; e = term { Negation e }
  | f = ID; LPAREN; args = separated_list(COMMA, exp); RPAREN
    { Function_call (f, args) }
  | e1 = term; OR; e2 = term { Or (e1, e2) }
  | e1 = term; AND; e2 = term { And (e1, e2) }
  | e1 = term; GE; e2 = term { Ge (e1, e2) }
  | e1 = term; GT; e2 = term { Gt (e1, e2) }
  | e1 = term; LE; e2 = term { Le (e1, e2) }
  | e1 = term; LT; e2 = term { Lt (e1, e2) }
  | e1 = term; NEQ; e2 = term { Neq (e1, e2) }
  | e1 = term; EQ; e2 = term { Eq (e1, e2) }
  | e1 = term; DIVIDE; e2 = term { Division (e1, e2) }
  | e1 = term; TIMES; e2 = term { Multiplication (e1, e2) }
  | e1 = term; MINUS; e2 = term { Subtraction (e1, e2) }
  | e1 = term; PLUS; e2 = term { Addition (e1, e2) }
  | t = ID; LBRACE; fields = separated_list(COMMA, separated_pair(ID, EQ, exp)); RBRACE
    { Record (t, fields) }
  | LPAREN; RPAREN { No_value }
  | LPAREN; e = exp; RPAREN { Parentheses e }
  | l = l_value { L_value l }
  ;

exp:
  | t = term { t }
  | IF; e1 = exp; THEN; e2 = term; ELSE; e3 = exp { If_then_else (e1, e2, e3) }
  | IF; e1 = exp; THEN; e2 = term; { If_then (e1, e2) }
  | WHILE; e1 = exp; DO; e2 = exp { While (e1, e2 )}
  | FOR; i = ID; e1 = assign; TO; e2 = exp; DO; e3 = exp
    { For (i, e1, e2, e3) }
  | BREAK { Break }
  | t = ID; LBRACK; e1 = exp; RBRACK; OF e2 = exp { Array (t, e1, e2) }
  | LET; decs = list(dec); IN; exps = separated_list(SEMICOLON, exp); END
    { Let (decs, exps) }
  | l = l_value; e = assign { Assignment (l, e) }
  ;

assign:
  ASSIGN; e = exp { e };

l_value:
  i = ID; l = list(l_value_access)
    { List.fold
        l
        ~init:(Id i)
        ~f:(fun accum ->
             function
             | `Record i -> Record_access(accum, i)
             | `Array e -> Array_access(accum, e))
    };

l_value_access:
  | DOT; i = ID { `Record i }
  | LBRACK; e = exp; RBRACK { `Array e }
  ;

dec:
  | t = tydec { Type_dec t }
  | v = vardec { Var_dec v }
  | f = func_dec { Func_dec f }
  ;

tydec:
  TYPE; name = ID; EQ; type_ = ty { { name; type_ } };

ty:
  | LBRACE; t = tyfields; RBRACE { Type.Record t }
  | ARRAY; OF; t = ID { Type.Array t }
  | t = ID { Type.Id t }
  ;

tyfields:
  t = separated_list(COMMA, separated_pair(ID, COLON, ID)) { t };

type_annotation:
  COLON; type_ = ID; { type_ }

vardec:
  VAR; var_name = ID; var_type_annotation = option(type_annotation); ASSIGN; var_content = exp
    { { var_name; var_type_annotation; var_content } };

func_dec:
  FUNCTION; func_name = ID; LPAREN; arguments = tyfields; RPAREN; func_type_annotation = option(type_annotation); EQ; func_content = exp
    { { func_name; arguments; func_type_annotation; func_content; } }
