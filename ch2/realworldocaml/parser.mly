(* taken from https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html *)
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token TRUE
%token FALSE
%token NULL
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token COLON
%token COMMA
%token EOF

%start <Json.t option> prog
%%

prog:
  | EOF       { None   }
  | v = value { Some v }
  ;

value:
  | LEFT_BRACE; obj = obj_fields; RIGHT_BRACE { `Assoc obj  }
  | LEFT_BRACK; vl = list_fields; RIGHT_BRACK { `List vl    }
  | s = STRING                                { `String s   }
  | i = INT                                   { `Int i      }
  | x = FLOAT                                 { `Float x    }
  | TRUE                                      { `Bool true  }
  | FALSE                                     { `Bool false }
  | NULL                                      { `Null       }
  ;

obj_fields:
  obj = separated_list(COMMA, obj_field) { obj };

obj_field:
  k = STRING; COLON; v = value { (k, v) };

list_fields:
  vl = separated_list(COMMA, value) { vl };
