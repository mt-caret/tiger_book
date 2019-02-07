open Core_kernel

val position_to_string : Lexing.position -> string
val error_of_string : Lexing.position -> string -> Error.t
val error_of_lexbuf : Lexing.lexbuf -> string -> Error.t
