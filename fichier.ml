open Syntaxe
exception Erreur_indentation of string 
exception Erreur_syntaxique of (int*int)
exception Not_comp
exception Not_name
exception Not_op
exception Not_mot_cle

let rec invers_list res=function
|[]->res
|e::l->invers_list (e::res) l


(*supprimme les espace d'une liste vide et retourner le nombre d'espace
et la liste sans les espaces *)
let rec supp_espace nb_espace=function
|[]->(nb_espace,[])
|""::l->supp_espace (nb_espace+1) l
|(e::l)as res->(nb_espace,res)



(*lire un fichier polish et retourne une liste de (nombre d'espace,liste des 
mots (lexique)) et leve une exception si l'indentaion n'est pas paire *)
let rec read_line fd_in i result : (int*(string list)) list=
     try
        let line=input_line fd_in in
        let liste_mot= String.split_on_char ' ' line in
        let nb_esp,list_sans_esp= supp_espace 0 liste_mot in
           if(nb_esp mod 2 = 1)then 
             raise (Erreur_indentation ("Erreur_indentation ligne " 
                                        ^ string_of_int i ) )
           else begin
             read_line fd_in (i+1) ((nb_esp,list_sans_esp)::result)
        end 
     with End_of_file->result



   

