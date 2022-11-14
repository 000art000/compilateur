open Syntaxe

val add :
  'a ->
  'b * 'b ->
  ('a * ('b * 'b)) list -> ('a * ('b * 'b)) list -> ('a * ('b * 'b)) list
  
val union :
  ('a * (bool * bool)) list ->
  ('a * (bool * bool)) list -> ('a * (bool * bool)) list
  
val vars_exp :
  expr -> (name * (bool * bool)) list -> (name * (bool * bool)) list
  
val vars_cond :
  expr * 'a * expr ->
  (name * (bool * bool)) list -> (name * (bool * bool)) list
  
val vars_bloc :
  block -> (name * (bool * bool)) list -> (name * (bool * bool)) list
  
val vars_ligne :
  position * instr ->
  (name * (bool * bool)) list -> (name * (bool * bool)) list

val string_of_vars :
  (string * ('a * bool)) list -> string -> string -> string  
