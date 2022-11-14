open Syntaxe

val add : 'a -> 'a list -> 'a list -> 'a list

val union : 'a list -> 'a list -> 'a list

val sign_op_add : sign -> sign list -> sign list -> sign list

val sign_op_sous : sign -> sign list -> sign list -> sign list

val sign_op_mult : sign -> sign list -> sign list -> sign list

val sign_op_div : sign -> sign list -> sign list -> sign list

val sign_op_mod : sign -> sign list -> sign list -> sign list

val get_variables_expr : expr -> name list

val get_variables_cond : expr * 'a * expr -> name list

val possible_egal : sign -> sign list -> bool

val possible_negal : sign -> sign list -> bool

val possible_lt : sign -> sign list -> bool

val possible_le : sign -> sign list -> bool

val possible_Gt : sign -> sign list -> bool

val possible_Ge : sign -> sign list -> bool

val nouveau_env :
  ('a * sign list) list -> ('a * sign list) list -> ('a * sign list) list
  
val invers_cond : 'a * comp * 'b -> 'a * comp * 'b 

val union_env :
  ('a * 'b list) list -> ('a * 'b list) list -> ('a * 'b list) list 
  
val egal_list : 'a list -> 'a list -> bool

val egal_env : ('a * 'b list) list -> ('a * 'b list) list -> bool 

val get_string_of_var : sign list -> string 

val get_string_of_signs : (string * sign list) list * string -> string 

