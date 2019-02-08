open Core_kernel

val position_to_string : Lexing.position -> string
val error_of_string : Lexing.position -> string -> Error.t
val or_error_of_string : Lexing.position -> string -> _ Or_error.t
val error_of_lexbuf : Lexing.lexbuf -> string -> Error.t
