open Core_kernel

let position_to_string (pos : Lexing.position) =
  sprintf
    "%s (line %d column %d)"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)
;;

let error_of_string (pos : Lexing.position) str =
  Error.of_string str |> Error.tag ~tag:(position_to_string pos)
;;

let or_error_of_string (pos : Lexing.position) str = Error (error_of_string pos str)
let error_of_lexbuf (lexbuf : Lexing.lexbuf) str = error_of_string lexbuf.lex_curr_p str
