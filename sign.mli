open Syntaxe
open Eval 
open Sign_extra

val sign_oper : sign list -> sign list -> sign list -> op -> sign list 

val sign_exp : expr -> (name * sign list) list -> sign list

val possibles : comp -> sign list -> sign list -> bool

val get_env_from_var :
  expr * comp * expr ->
  name ->
  sign list ->
  (name * sign list) list -> sign list -> int -> string -> sign list * string 
  
val get_env_from_cond :
  expr * comp * expr ->
  name list ->
  (name * sign list) list ->
  (name * sign list) list ->
  string -> int -> (name * sign list) list * string
  
val sign_if_while :
  expr * comp * expr ->
  (name * sign list) list ->
  string -> int -> (bool * (name * sign list) list) list * string
  
val sign_while :
  cond ->
  block ->
  (name * sign list) list ->
  string -> position -> (name * sign list) list * string
  
val sign_bloc :
  block ->
  (name * sign list) list -> string -> (name * sign list) list * string
  
val sign_ligne :
  position * instr ->
  (name * sign list) list -> string -> (name * sign list) list * string
  
val affichage : block -> string 
