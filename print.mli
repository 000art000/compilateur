open Syntaxe

val string_of_indentation : int -> string ->string
val get_string_exp : expr -> string
val get_string_cond : cond->string
val get_string_bloc : block ->int-> string 
val get_string_bloc : block->int->string
val get_string_ligne : (position * instr)-> int ->string
