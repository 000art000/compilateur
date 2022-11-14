open Syntaxe
open Fichier

val raise_excp_syn : int -> int -> 'a 

val is_mot_cle : string -> unit 

val is_comp : string -> comp 

val is_op : string -> op 

val is_name : string -> string 

val is_read : int -> int -> string list -> string

val is_name_or_int : int -> int -> name -> expr

val len : expr -> int 

val avancer : 'a list -> int -> 'a list 

val is_expr2 : int -> int -> name list -> expr 

val is_expr : int -> int -> name list -> expr 

val is_cond : int -> int -> name list -> cond 

val is_set : int -> int -> name list -> instr 

val len_ligne : block -> int 

val if_to_prog : int -> int -> (int * string list) list -> name list -> instr 

val is_while : int -> int -> (int * string list) list -> name list -> instr 

val convert_to_program : int -> int -> (int * string list) list -> program 

val read_file : string -> program 


