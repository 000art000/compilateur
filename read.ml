open Syntaxe
open Fichier

(* lever une exeption syntaxique a une ligne et colone donné *)
let raise_excp_syn lig col =
raise (Erreur_syntaxique (lig,col))

(* verifier si un string est un mot clé *)
let is_mot_cle=function
|"IF"->()
|"ELSE"->()
|"WHILE"->()
|"PRINT"->()
|"READ"->()
|_->raise (Not_mot_cle)

(* verifier si un string est un oprateur de comparaison *)
let is_comp=function
|"="->Eq
|"<>"->Ne
|"<"->Lt
|"<="->Le
|">"->Gt
|">="->Ge
|_->raise (Not_comp)

(* verifier si un string est un oprateur aritméthique *)
let is_op=function
|"+"->Add
|"-"->Sub
|"*"->Mul
|"/"->Div
|"%"->Mod
|_->raise (Not_op)


(*verifier si une chaine de caractere n'est pas un mot reservé 
(verifier qu'il sagit d'une variable)*)
let is_name mot=
try
  let _ = int_of_string mot in raise Not_name
with Failure a->(
  try
  let _ = is_op mot in raise Not_name
  with Not_op->(
    try
    let _ = is_comp mot in raise Not_name
    with Not_comp->(
      try
      let _ = is_mot_cle mot in raise Not_name 
      with Not_mot_cle-> mot
   )
  )
)


(*verifier qu'il sagit d'une seule variable apres le read*)
let is_read lig col=function
|mot::[]->(try is_name mot with Not_name->raise_excp_syn lig col)
|_->raise_excp_syn lig col
;;
(* verifier qu'il sagit d'une variable ou d'une valeur entiere *)
let is_name_or_int lig col mot =
try let i=int_of_string mot in Num (i) with Failure a->(
                 try let _=is_name mot in Var (mot)
                 with Not_name ->raise_excp_syn lig col)

(*fonction qui calcule la taille d'une expression*)
let rec len expression=
    match expression with
    | Op(operateur,expr1,expr2)->1+(len expr1)+(len expr2)
    |_->1
    
(*fonction pour sauter de n lignes dans une liste de lignes*)
let rec avancer l n=
match l with
|[]->[]
|x::l'->if n=0 then l else avancer l' (n-1)

(* verifier si une expression exite sans verifier ce qu'il ya apres*)
let rec is_expr2 lig col =function
|[]->raise_excp_syn lig col
|mot::l->try is_name_or_int lig col mot 
         with Erreur_syntaxique (i,j)->
             (let oper= is_op mot in let exp1=is_expr2 lig col l in
let taille_exp1=(len exp1) in
           let exp2=is_expr2 lig col (avancer l taille_exp1) in
           Op(oper,exp1,exp2) )


(* verifier si une expression avec l'obligation d'avoir une liste vide 
apres cette expression*)
let is_expr lig col =function
|[]->raise_excp_syn lig col
|mot::[]->is_name_or_int lig col mot 
|op::rest->let oper= is_op op in let exp1=is_expr2 lig col rest in
           let taille_exp1=(len exp1)in
           let exp2=is_expr2 lig col (avancer rest taille_exp1)in
           if (avancer rest (taille_exp1+(len exp2)))=[] 
               then Op(oper,exp1,exp2) 
           else raise_excp_syn lig col

(* verifier s'il sagit d'une condition *)
let is_cond lig col list_mot=
match list_mot with
|[]->raise_excp_syn lig col
|mot::lm->(let expr1=is_expr2 lig col list_mot in 
          let taille_epxr1=len expr1 in
          let apr_expr1=avancer list_mot (taille_epxr1) in
          match apr_expr1 with
          |[]->raise_excp_syn lig col
          |comp::rest->try let cmp=is_comp comp in 
                       let expr2= is_expr lig col rest 
                       in  (expr1,cmp,expr2)
           with Not_comp ->raise_excp_syn lig col
          )
(* verifier s'il sagit une affectation *)         
let is_set lig col lm=
match lm with 
|mot::":="::l->(try
 let var=is_name mot in let expr=is_expr lig col l in Set(var,expr)  
with not_name->raise_excp_syn lig col)
|_->raise_excp_syn lig col

(* calculé la taille d'un block *)
let rec len_ligne program=
   match program with
    |[]->0
    | (_,Set(nom,express))::l->1+(len_ligne l)
    | (_,Read(nom))::l->1+(len_ligne l)
    | (_,Print(express))::l->1+(len_ligne l)
    | (_,If(condit,bloc1,bloc2))::l->
               if bloc2=[] then (len_ligne bloc1)+(len_ligne l)+1 
               else 2+(len_ligne bloc1)+(len_ligne bloc2)+(len_ligne l)
    | (_,While(condit,bloc1))::l->1+(len_ligne bloc1)+(len_ligne l)

(* verifier s'il sagit d'un block if et probablement avec un else  *)   
let rec if_to_prog lig inde l lm2 =
    let cond=is_cond lig (inde) lm2 in
    let block1=convert_to_program (lig+1) (inde+2) l in
    let apres_avancer=avancer l (len_ligne block1) in
    if apres_avancer <>[] then
    let apres_if=(List.hd (apres_avancer))in
    if (fst apres_if)=inde then
    if (List.hd (snd apres_if))="ELSE" then
    if List.length (snd apres_if)=1 then
    let block2=convert_to_program (lig+(len_ligne block1)+2) (inde+2) 
                                  (avancer l ((len_ligne block1) +1)) 
    in (If(cond,block1,block2)) 
    else raise_excp_syn lig inde 
    else If(cond,block1,[])
    else If(cond,block1,[])
    else If(cond,block1,[])
                      
and       

(* verfier s'il s'agit d'une boucle *)                      
is_while lig inde l lm2=
 let cond=is_cond lig (inde) lm2 in
    let block=convert_to_program (lig+1) (inde+2) l in
    While(cond,block)

and
                      
(* convertir la liste lus depuis le fichier polish vers un program *)                      
convert_to_program lig inde list_lu =
   match list_lu with
   |[]->[]
   |(esp,list_mot)::l->
         if esp=inde then 
            match list_mot with
            |[]->convert_to_program (lig+1) inde l
            |"READ"::lm2->let mot=is_read lig (esp+4) lm2 in
                          (lig,Read mot)::(convert_to_program (lig+1) inde l)
            |"IF"::lm2->
               let if_bloc=if_to_prog lig inde l lm2 in
               (match if_bloc with
                |If(cond,bloc1,bloc2)->
                    let size=(len_ligne bloc1) +(len_ligne bloc2)in 
                    if bloc2 =[] then 
                      (lig,if_bloc)::(convert_to_program (lig+size+1) inde 
                                       (avancer l (size)))
                    else (lig,if_bloc)::(convert_to_program (lig+size+2) inde 
                                       (avancer l (size+1)))
                |_->raise_excp_syn lig inde)
            |"WHILE"::lm2->let while_block=is_while lig inde l lm2 in
               (match while_block with
                |While(cond,block)->let size=(len_ligne block) in
                      (lig,while_block)::(convert_to_program (lig+size+1) inde 
                                         (avancer l size))
                |_->raise_excp_syn lig inde)
            |"PRINT"::lm2-> (lig,Print(is_expr lig (esp+5) lm2))
                                 ::(convert_to_program (lig+1) inde l)
            |"COMMENT"::lm2->convert_to_program (lig+1) inde l
            |x::lm2->let set=is_set lig inde list_mot
              in (lig,set)::convert_to_program (lig+1) inde l
   
         else if (esp>inde) then raise_excp_syn lig esp 
         else []


(* la fonction qui lit le fichier polish et le covertit vers un programme  *)
let read_file filename =  
   try 
     let fd_in=open_in filename in
     let list_line= invers_list [] (read_line fd_in 1 [])
     in  convert_to_program 1 0 list_line
   with 
   |Sys_error a-> Printf.printf "droit d'accés au fichier invalide";exit (-1)
   |Erreur_indentation s->Printf.printf "%s" s;exit (-1)
   |Erreur_syntaxique (i,j)->
          Printf.printf "erreur syntaxique ligne %i colonne %i" i j;exit (-1)
   

