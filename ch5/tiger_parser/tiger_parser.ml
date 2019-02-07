open Core
open Async
open Semant

let attempt_parse (lexbuf : Lexing.lexbuf) =
  Result.try_with (fun () -> Parser.program Lexer.read lexbuf)
  |> Result.map_error ~f:(function
         | Lexer.SyntaxError msg -> Util.error_of_lexbuf lexbuf msg
         | Parser.Error -> Util.error_of_lexbuf lexbuf "syntax error"
         | x -> Util.error_of_lexbuf lexbuf (Exn.to_string x) )
;;

let run_parser ~filename =
  let%map source_code = Reader.with_file filename ~f:Reader.contents in
  let lexbuf = Lexing.from_string source_code in
  attempt_parse lexbuf
;;

let run_parser_param =
  let open Command.Let_syntax in
  let%map_open filename = anon ("filename" %: file) in
  fun () -> run_parser ~filename >>| printf !"%{sexp:Absyn.exp Or_error.t}\n"
;;

let () =
  Command.async
    ~summary:"Parses source code for the Tiger programming language"
    run_parser_param
  |> Command.run
;;
