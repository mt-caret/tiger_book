open Core
open Async

(*open Absyn*)

let position_to_string (pos : Lexing.position) =
  sprintf
    "%s (line %d column %d)"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)
;;

let create_error_string (lexbuf : Lexing.lexbuf) error_message =
  let pos_str = position_to_string lexbuf.lex_curr_p in
  sprintf "%s: %s" pos_str error_message
;;

let attempt_parse (lexbuf : Lexing.lexbuf) =
  Result.try_with (fun () -> Parser.program Lexer.read lexbuf)
  |> Result.map_error ~f:(function
         | Lexer.SyntaxError msg -> create_error_string lexbuf msg
         | Parser.Error -> create_error_string lexbuf "syntax error"
         | x -> create_error_string lexbuf (Exn.to_string x) )
;;

let run_parser ~filename =
  let%map source_code = Reader.with_file filename ~f:Reader.contents in
  let lexbuf = Lexing.from_string source_code in
  attempt_parse lexbuf
;;

let run_parser_param =
  let open Command.Let_syntax in
  let%map_open filename = anon ("filename" %: file) in
  fun () -> run_parser ~filename >>| printf !"%{sexp:(Absyn.exp, string) Result.t}\n"
;;

let () =
  Command.async
    ~summary:"Parses source code for the Tiger programming language"
    run_parser_param
  |> Command.run
;;
