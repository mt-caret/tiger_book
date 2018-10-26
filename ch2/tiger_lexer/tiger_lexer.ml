open Core
open Async

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

let run_lexer ~filename =
  let%map source_code = Reader.with_file filename ~f:Reader.contents in
  let lexbuf = Lexing.from_string source_code in
  let rec lex accum =
    let open Result.Let_syntax in
    match%bind attempt_lex lexbuf with
    | Tokens.EOF -> Ok (List.rev (Tokens.EOF :: accum))
    | token -> lex (token :: accum)
  in
  lex []
;;

let run_lexer_param =
  let open Command.Let_syntax in
  let%map_open filename = anon ("filename" %: file) in
  fun () ->
    run_lexer ~filename
    >>| printf !"%{sexp:(Tokens.token list, string) Result.t}\n"
;;

let () =
  Command.async
    ~summary:"Lexes source code for the Tiger programming language"
    run_lexer_param
  |> Command.run
;;

