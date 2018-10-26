open Core
open Async
(* open Lexer
 * open Lexing *)

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

let run_lexer ~filename () =
  let%map source_code = Reader.with_file filename ~f:Reader.contents in
  let lexbuf = Lexing.from_string source_code in
  let rec lex accum =
    let open Result.Let_syntax in
    match%bind attempt_lex lexbuf with
    | Tokens.EOF -> Ok (Tokens.EOF :: accum)
    | token -> lex (token :: accum)
  in
  lex []
  |> Result.map ~f:List.rev
  |> printf !"%{sexp:(Tokens.token list, string) Result.t}\n"
;;

let run_lexer_param =
  let open Command.Let_syntax in
  let%map_open filename = anon ("filename" %: file) in
  run_lexer ~filename
;;

let () =
  Command.async
    ~summary:"Lexes source code for the Tiger programming language"
    run_lexer_param
  |> Command.run
;;

(* let print_position outx lexbuf =
 *   let pos = lexbuf.lex_curr_p in
 *   fprintf outx "%s:%d:%d" pos.pos_fname
 *     pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
 * ;;
 * 
 * let parse_with_error lexbuf =
 *   try Parser.prog Lexer.read lexbuf with
 *   | SyntaxError msg ->
 *     fprintf stderr "%a: %s\n" print_position lexbuf msg;
 *     None
 *   | Parser.Error ->
 *     fprintf stderr "%a: syntax error\n" print_position lexbuf;
 *     exit (-1)
 * ;;
 * 
 * let rec parse_and_print lexbuf =
 *   match parse_with_error lexbuf with
 *   | Some value ->
 *     Json.output_value value;
 *     parse_and_print lexbuf
 *   | None -> ()
 * ;;
 * 
 * let loop filename () =
 *   let inx = In_channel.create filename in
 *   let lexbuf = Lexing.from_channel inx in
 *   lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
 *   parse_and_print lexbuf;
 *   In_channel.close inx
 * ;;
 * 
 * let () =
 *   Command.basic_spec ~summary:"Parse and display JSON"
 *     Command.Spec.(empty +> anon ("filename" %: file))
 *     loop
 *   |> Command.run
 * ;; *)
