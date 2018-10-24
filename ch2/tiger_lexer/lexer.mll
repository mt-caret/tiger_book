{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']
let int = ['1'-'9'] digit*
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | whitespace { read lexbuf }
  | newline    { next_line lexbuf; read lexbuf }
  | int        { INT (Int.of_string (Lexing.lexeme lexbuf)) }
  | "type"     { TYPE }
  | "var"      { VAR }
  | "function" { FUNCTION }
  | "break"    { BREAK }
  | "of"       { OF }
  | "end"      { END }
  | "in"       { IN }
  | "nil"      { NIL }
  | "let"      { LET }
  | "do"       { DO }
  | "to"       { TO }
  | "for"      { FOR }
  | "while"    { WHILE }
  | "else"     { ELSE }
  | "then"     { THEN }
  | "if"       { IF }
  | "array"    { ARRAY }
  | id         { ID (Lexing.lexeme lexbuf) }
  | "/*"       { read_comment lexbuf; read lexbuf }
  | '"'        { read_string (Buffer.create 16) lexbuf }
  | ":="       { ASSIGN }
  | '|'        { OR }
  | '&'        { AND }
  | ">="       { GE }
  | '>'        { GT }
  | "<="       { LE }
  | '<'        { LT }
  | "<>"       { NEQ }
  | '='        { EQ }
  | '/'        { DIVIDE }
  | '*'        { TIMES }
  | '-'        { MINUS }
  | '+'        { PLUS }
  | '.'        { DOT }
  | '}'        { RBRACE }
  | '{'        { LBRACE }
  | ']'        { RBRACK }
  | '['        { LBRACK }
  | ')'        { RPAREN }
  | '('        { LPAREN }
  | ';'        { SEMICOLON }
  | ':'        { COLON }
  | ','        { COMMA }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof        { EOF }

and read_comment =
  parse
  | "*/" { () }
  | "/*" { read_comment lexbuf; read_comment lexbuf }
  | _ { read_comment lexbuf }
  | eof { raise (SyntaxError ("Comment is not terminated")) }

and read_string buf =
  parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  (* TODO *)
  | _ { raise (SyntaxError "Unimplemented!") }
  | eof { raise (SyntaxError ("String is not terminated")) }

(*
rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "null"   { NULL }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | '{'      { LEFT_BRACE }
  | '}'      { RIGHT_BRACE }
  | '['      { LEFT_BRACK }
  | ']'      { RIGHT_BRACK }
  | ':'      { COLON }
  | ','      { COMMA }
  | eof      { EOF }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
*)
