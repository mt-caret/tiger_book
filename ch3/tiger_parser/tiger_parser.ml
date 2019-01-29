open Core
open Async

(*
let attempt_lex (lexbuf : Lexing.lexbuf) =
  Result.try_with (fun () -> Lexer.read lexbuf)
  |> Result.map_error ~f:(function
      | Lexer.SyntaxError msg ->
        let pos = lexbuf.lex_curr_p in
        sprintf "%s (line %d column %d): %s"
          pos.pos_fname
          pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol + 1)
          msg
      | x ->
        Exn.to_string x
    )
;;

let run_lexer ~filename =
  let%map source_code = Reader.with_file filename ~f:Reader.contents in
  let lexbuf = Lexing.from_string source_code in
  let rec lex accum =
    let open Result.Let_syntax in
    match%bind attempt_lex lexbuf with
    | Parser.EOF -> Ok (List.rev (Parser.EOF :: accum))
    | token -> lex (token :: accum)
  in
  lex []
;;
*)

let create_error_string (lexbuf : Lexing.lexbuf) error_message =
  let pos = lexbuf.lex_curr_p in
  sprintf "%s (line %d column %d): %s"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)
    error_message
;;

let attempt_parse (lexbuf : Lexing.lexbuf) =
  Result.try_with (fun () -> Parser.exp Lexer.read lexbuf)
  |> Result.map_error ~f:(function
      | Lexer.SyntaxError msg ->
        create_error_string lexbuf msg
      | Parser.Error ->
        create_error_string lexbuf "syntax error"
      | x ->
        Exn.to_string x
    )
;;

let run_parser ~filename =
  let%map source_code = Reader.with_file filename ~f:Reader.contents in
  let lexbuf = Lexing.from_string source_code in
  attempt_parse lexbuf
;;

let run_parser_param =
  let open Command.Let_syntax in
  let%map_open filename = anon ("filename" %: file) in
  fun () ->
    run_parser ~filename
    >>| printf !"%{sexp:(Tiger.exp, string) Result.t}"
;;

let () =
  Command.async
    ~summary:"Parses source code for the Tiger programming language"
    run_parser_param
  |> Command.run
;;

