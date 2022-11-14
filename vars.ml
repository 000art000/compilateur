open Syntaxe

(* fonction qui rajoute un nouveau element "nom" avec la valeur "valeur" 
si il n'exite pas  *)
let rec add nom valeur variables variables_prec =
    match variables with
    |[]->(nom,valeur)::variables_prec
    |(var,vall)::nxt->if var=nom then 
                List.rev_append ((var,(fst vall,fst vall))::nxt) variables_prec  
                      else add nom valeur nxt ((var,vall)::variables_prec) 

(* fonction qui fait l'union entre deux environements *)    
let rec union variables1 variables2=
   (*fonction auxiliaire qui rajoute un element avec un ecrasement
   (des && logique entre les nouvelles valeurs et les valeurs precedente) 
   s'il exite sinon elle rajoute avec le premier element du tuple false *)
   let rec add_avec_ecrasement nom valeur variables variables_prec =
       match variables with
       |[]->(nom,(false,snd valeur))::variables_prec
       |(var,(vall1,vall2))::nxt->if var=nom then 
             (nom,(fst valeur && vall1,snd valeur && vall2))::
                                 (List.rev_append variables_prec nxt)   
                                  else 
             add_avec_ecrasement nom valeur nxt 
                                 ((var,(vall1,vall2))::variables_prec)
   in
   match variables1 with
   |[]->variables2
   |(var,vall)::nxt->union nxt (add_avec_ecrasement var vall variables2 [])

(* fonction qui prend une expression et un environnement et retourne 
le nouveau environnement correspondant *)
let rec vars_exp expression variables =
    match expression with
    | Num(num)->variables
    | Var(nom)->add nom (false,false) variables []
    | Op(operateur,expr1,expr2)->let variables =vars_exp expr1 variables in 
                                  vars_exp expr2 variables
    
(* fonction qui prend une condition et un environnement et retourne 
le nouveau environnement correspondant *)
let vars_cond condition variables=
    match condition with
    |(exp1,_,exp2)->let variables=vars_exp exp1 variables in 
                       vars_exp exp2 variables

(* fonction qui prend un bloc et un environnement et retourne 
le nouveau environnement correspondant *)
let rec vars_bloc b variables= 
    (match b with
    |[]->variables
    |ligne::b_suiv->let variables=(vars_ligne ligne variables)in
                                   vars_bloc b_suiv variables)
(* fonction qui prend une ligne et un environnement et retourne 
le nouveau environnement correspondant *)
and vars_ligne ligne variables=
    match (snd ligne) with
    | Set(nom,express)->let variables =vars_exp express variables in 
                                  add nom (true,true) variables []
    | Read(nom)->add nom (true,true) variables []
    | Print(express)->vars_exp express variables 
    | If(condit,bloc1,bloc2)->let variables=vars_cond condit variables in 
                              let variables1=vars_bloc bloc1 variables in
                              let variables2=vars_bloc bloc2 variables in 
                              union variables1 variables2
    | While(condit,bloc1)->let variables=vars_cond condit variables in 
                              union (vars_bloc bloc1 variables) variables
    
(* fonction qui prend des variables en parametres et retourne 
le string qui sera affiché *)
let rec string_of_vars variables str1 str2=    
  match variables with
  |[]->str1^"\n"^str2^"\n"
  |(var,(_,vall))::nxt->if vall=true then string_of_vars nxt (var^" "^str1) str2 
                        else string_of_vars nxt (var^" "^str1) (var^" "^str2)
    
    
