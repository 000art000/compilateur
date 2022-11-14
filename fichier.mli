exception Erreur_indentation of string 
exception Erreur_syntaxique of (int*int)
exception Not_comp
exception Not_name
exception Not_op
exception Not_mot_cle

val invers_list : 'a list ->'a list ->'a list

val supp_espace : int ->string list ->int * string list

val read_line : in_channel ->int ->(int*string list) list  -> (int*string list) list 
