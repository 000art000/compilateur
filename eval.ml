open Syntaxe
(*fonction auxiliaire qui execute l'affectation*)
let rec set_name nom valeur variables variables_prec =
    match variables with
    |[]->(nom,valeur)::variables_prec
    |(var,vall)::nxt->if var=nom then 
                    (nom,valeur)::(List.rev_append variables_prec nxt) 
                    else set_name nom valeur nxt ((var,vall)::variables_prec) 

(*fonction auxiliaire pour evaluer une expression*)
let rec eval_exp expression variables=
    match expression with
    | Num(num)->num
    | Var(nom)->(try List.assoc nom variables 
                 with Not_found -> (failwith "variable non initialisé"))
    | Op(operateur,expr1,expr2)->let res1=(eval_exp expr1 variables) and 
                                 res2=(eval_exp expr2 variables)in
                                  match operateur with
                                  | Add ->res1+res2
                                  | Sub ->res1-res2
                                  | Mul ->res1*res2
                                  | Div ->if res2<>0 then res1/res2 
                                       else failwith "erreur division par zero"
                                  | Mod ->if res2<>0 then res1 mod res2 
                                         else failwith "erreur modulo par zero"

(* fonction auxiliaire pour evaluer une condition *)
let eval_cond condition variables=
    match condition with
    |(exp1,Eq,exp2)->(eval_exp exp1 variables)=(eval_exp exp2 variables)
    |(exp1,Ne,exp2)->(eval_exp exp1 variables)!=(eval_exp exp2 variables)
    |(exp1,Lt,exp2)->(eval_exp exp1 variables)<(eval_exp exp2 variables)
    |(exp1,Le,exp2)->(eval_exp exp1 variables)<=(eval_exp exp2 variables)
    |(exp1,Gt,exp2)->(eval_exp exp1 variables)>(eval_exp exp2 variables)
    |(exp1,Ge,exp2)->(eval_exp exp1 variables)>=(eval_exp exp2 variables)

(* fonction auxiliare pour avaluer un bloc*)
let rec eval_bloc b variables = 
    (match b with
    |[]->variables
    |ligne::b_suiv->let variables=(eval_ligne ligne variables)in
                                   eval_bloc b_suiv variables)
                                   
(* fonction auxiliaire pour traiter les lignes et les evaluer*)
and eval_ligne ligne variables=
    match (snd ligne) with
    | Set(nom,express)->set_name nom (eval_exp express variables) variables []
    | Read(nom)->let x=read_int () in set_name nom x variables []
    | Print(express)->(Printf.printf "%d\n" (eval_exp express variables);
                       variables)
    | If(condit,bloc1,bloc2)->if (eval_cond condit variables) 
                                 then eval_bloc bloc1 variables
                              else eval_bloc bloc2 variables
    | While(condit,bloc1)->if (eval_cond condit variables) 
                             then eval_ligne ligne (eval_bloc bloc1 variables) 
                           else variables
    
    
