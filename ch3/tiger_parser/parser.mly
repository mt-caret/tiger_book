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
open Tiger
%}

%start <Tiger.exp> exp
%%

program:
  | e = exp; EOF { e }
  ;

exp:
  LET; decs = list(dec); IN; exps = separated_list(SEMICOLON, exp); END { Let (decs, exps) };

dec:
  | t = tydec { Type_dec t }
  | v = vardec { Var_dec v }
  ;

tydec:
  TYPE; name = ID; EQ; type_ = ty { { name; type_ } };

ty:
  | LBRACE; t = tyfields; RBRACE { Tiger.Type.Record t }
  | ARRAY; OF; t = ID { Type.Array t }
  | t = ID { Type.Id t }
  ;

tyfields:
  t = separated_list(COMMA, separated_pair(ID, COLON, ID)) { t };

(*
vardec:
  | VAR; var_name = ID; COLON; type_ = ID; ASSIGN; var_content = exp { { var_name; var_type_annotation = Some type_; var_content } }
  | VAR; var_name = ID; ASSIGN; var_content = exp { { var_name; var_type_annotation = None; var_content } }
*)

type_annotation:
  COLON; type_ = ID; { type_ }

vardec:
  VAR; var_name = ID; var_type_annotation = option(type_annotation); ASSIGN; var_content = exp
    { { var_name; var_type_annotation; var_content } };
