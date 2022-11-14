open Syntaxe
 
val set_name : name -> int -> ( string * int ) list -> (string*int)list -> (string*int)list

val eval_exp : expr -> (string*int)list ->int

val eval_cond : cond -> (string*int)list -> bool

val eval_bloc : block -> (string*int)list -> (string*int)list

val eval_ligne : (position * instr) -> (string*int)list -> (string*int)list
