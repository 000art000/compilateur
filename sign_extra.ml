open Syntaxe

(* ajouter un element à la liste si il n'existe pas*)
let rec add elem liste acc=
	match liste with
	|[]->elem::acc
	|ele::nxt->if ele=elem then liste@acc else add elem nxt (ele::acc)

(* fusioner 2 liste *)	
let rec union list1 list2=
	match list1 with
	|[]->list2
	|ele::next->union next (add ele list2 [])
	
(* retourner un liste des signes possibles pour l'operation d'addtion avec 
un signe de l'expression 1 et une liste de signes de l'expression 2*)	
let rec sign_op_add s sign_exp2 acc=
	match s,sign_exp2 with
	|_,[]->acc
	|Zero,_->sign_exp2
	|Error,_->add Error sign_exp2 []
	|Neg,Pos::nxt->
	     sign_op_add Neg nxt (add Zero (add Pos (add Neg acc []) []) [])
	|Pos,Neg::nxt->
	     sign_op_add Pos nxt (add Zero (add Pos (add Neg acc []) []) [])
	|x,Error::nxt->
	     sign_op_add x nxt (add Error acc [])
	|x,_::nxt->sign_op_add x nxt (add x acc [])

(* retourner une liste des signes possibles pour l'operation de soustraction avec 
un signe de l'expression 1 et une liste de signes de l'expression 2 *)	
let rec sign_op_sous s sign_exp2 acc=
	match s,sign_exp2 with
	|_,[]->acc
	|Zero,Pos::nxt->sign_op_sous Zero nxt (add Neg acc [])
	|Zero,Neg::nxt->sign_op_sous Zero nxt (add Pos acc [])
	|Zero,x::nxt->sign_op_sous Zero nxt (add x acc [])
	|Error,_->add Error sign_exp2 []
	|x,Error::nxt->sign_op_sous x nxt (add Error acc [])
	|Neg,Neg::nxt->
	    sign_op_sous Neg nxt (add Zero (add Pos (add Neg acc []) []) [])
	|Pos,Pos::nxt->
	    sign_op_sous Pos nxt (add Zero (add Pos (add Neg acc []) []) [])
	|x,_::nxt->
	    sign_op_sous x nxt (add x acc [])

(* retourner une liste des signes possibles pour l'operation de multiplication avec 
un signe de l'expression 1 et une liste de signes de l'expression 2 *)	
let rec sign_op_mult s sign_exp2 acc=
	match s,sign_exp2 with
	|_,[]->acc
	|Error,_->add Error sign_exp2 []
	|x,Error::nxt->sign_op_mult x nxt (add Error acc [])
	|Zero,_::nxt->sign_op_mult Zero nxt (add Zero acc [])
	|x,Zero::nxt->sign_op_mult x nxt (add Zero acc [])
	|Pos,Pos::nxt->sign_op_mult Pos nxt (add Pos acc [])
	|Neg,Neg::nxt->sign_op_mult Neg nxt (add Pos acc [])
	|x,y::nxt->sign_op_mult x nxt (add Neg acc [])

(* retourner une liste des signes possibles pour l'operation de division avec 
un signe de l'expression 1 et une liste de signes de l'expression 2 *)	
let rec sign_op_div s sign_exp2 acc=
	match s,sign_exp2 with
	|_,[]->acc
	|Error,_->add Error sign_exp2 []
	|x,Error::nxt->sign_op_div x nxt (add Error acc [])
	|x,Zero::nxt->sign_op_div x nxt (add Error acc [])
	|Zero,_::nxt->sign_op_div Zero nxt (add Zero acc [])
	|Pos,Pos::nxt->sign_op_div Pos nxt (add Pos (add Zero acc []) [])
	|Neg,Neg::nxt->sign_op_div Neg nxt (add Pos (add Zero acc []) [])
	|x,y::nxt->sign_op_div x nxt (add Neg (add Zero acc []) [])

(* retourner une liste des signes possibles pour l'operation de modulo avec 
un signe de l'expression 1 et une liste de signes de l'expression 2 *)	
let rec sign_op_mod s sign_exp2 acc=
	match s,sign_exp2 with
	|_,[]->acc
	|Error,_->add Error sign_exp2 []
	|x,Error::nxt->sign_op_mod x nxt (add Error acc [])
	|x,Zero::nxt->sign_op_mod x nxt (add Error acc [])
	|Zero,_::nxt->sign_op_mod Zero nxt (add Zero acc [])
	|Pos,_::nxt->sign_op_mod Pos nxt (add Pos (add Zero acc []) [])
	|Neg,_::nxt->sign_op_mod Neg nxt (add Neg (add Zero acc []) [])	

(* prend en parametre une expression et retourne une liste des variables 
utilisé dans l'expression *)	
let rec get_variables_expr=function	
	|Num(c)->[]
	|Var(v)->[v]
	|Op(op,ex1,ex2)->union (get_variables_expr ex1) (get_variables_expr ex2)

(* prend en parametre une condition et retourne une liste des variables 
utilisé dans la condition *)
let  get_variables_cond cond=
	match cond with 
	|ex1,op,ex2->union (get_variables_expr ex1) (get_variables_expr ex2)

(* verifer si une condition de l'egalité est possible, elle prend en parametre 
un signe de la premiere expression et la liste des signes de la 2eme expression*)
let possible_egal ele signe_exp2=
	if ele <> Error then List.mem ele signe_exp2 else false 

(* fonction qui verifie si on peut entrer dans une condition de non egalité 
elle prend un signe de l'expression 1 de la condtion et la liste des signes de l'expression 2 *)
let possible_negal ele signe_exp2=
	if ele =Zero then 
	(match signe_exp2 with
		    		|Zero::[]->false
		    		|Error::[]->false
		    		|Zero::Error::[]->false
		    		|Error::Zero::[]->false
		    		|_->true)
         else if ele=Error then false else (
            match signe_exp2 with
		    		|Error::[]->false
		    		|_->true
         )
		    	
(* fonction qui verifie si on peut entrer dans une condition de inferieure
elle prend un sign de l'expression 1 de la condtion et la liste des signes de 
l'expression 2 *)		    	
let rec possible_lt ele signe_exp2=
	match ele,signe_exp2 with
	|_,[]->false
	|Error,_->false
	|_,Error::l->possible_lt ele l
	|Neg,_::l->true
	|_,Pos::l->true
	|_,_::l->possible_lt ele l	

(* fonction qui verifie si on peut entrer dans une condition de inferieure-ou-egal
elle prend un sign de l'expression 1 de la condtion et la liste des signes de 
l'expression 2 *)
let rec possible_le ele signe_exp2=
	match ele,signe_exp2 with
	|_,[]->false
	|Error,_->false
	|_,Error::l->possible_le ele l
	|Neg,_::l->true
	|Zero,Zero::l->true
	|_,Pos::l->true
	|_,_::l->possible_le ele l

(* fonction qui verifie si on peut entrer dans une condition de superieure
elle prend un sign de l'expression 1 de la condtion et la liste des signes de 
l'expression 2 *)
let rec possible_Gt ele signe_exp2=
	match ele,signe_exp2 with
	|_,[]->false
	|Error,_->false
	|_,Error::l->possible_Gt ele l
	|Pos,_::l->true
	|_,Neg::l->true
	|_,_::l->possible_Gt ele l

(* fonction qui verifie si on peut entrer dans une condition de superieure-ou-egal
elle prend un sign de l'expression 1 de la condtion et la liste des signes de 
l'expression 2 *)
let rec possible_Ge ele signe_exp2=
	match ele,signe_exp2 with
	|_,[]->false
	|Error,_->false
	|_,Error::l->possible_Ge ele l
	|Pos,_::l->true
	|Zero,Zero::l->true
	|_,Neg::l->true
	|_,_::l->possible_Ge ele l

(* fonction prend un environement globale et l'environement qui satisfait une 
condition et qui contient que les variables de la condition et retourne 
l'environement  qui satisfait la condition avec toutes les variables*)	
let rec nouveau_env variables liste=
	match liste with
	|[]->variables
	|(ele,l)::nxt->if List.mem Error (List.assoc ele variables) then
	nouveau_env ((ele,Error::l)::(List.remove_assoc ele variables)) nxt
	else 
	nouveau_env ((ele,l)::(List.remove_assoc ele variables)) nxt

(*fonction qui inverse une condition*)
let invers_cond =function
	|ex1,Eq,ex2->ex1,Ne,ex2
	|ex1,Ne,ex2->ex1,Eq,ex2
	|ex1,Lt,ex2->ex1,Ge,ex2
	|ex1,Le,ex2->ex1,Gt,ex2
	|ex1,Gt,ex2->ex1,Le,ex2
	|ex1,Ge,ex2->ex1,Lt,ex2

(* function qui prend deux environement et les fusione*)               
let rec union_env env1 env2=
	match env1 with
	|[]->env2
	|ele::next->try union_env next ((fst ele,union (snd ele)
	 (List.assoc (fst ele) env2))::(List.remove_assoc (fst ele) env2))
		    with Not_found -> union_env next (ele::env2)

(* fonction qui verifie si deux listes de signes contient les memes signes 
"l'ordre ne compte pas" *)
let rec egal_list l1 l2=
	match l1 with
	|[]->if l2=[] then true else false 
	|ele::next->if List.mem ele l2 then egal_list next (List.filter_map 
	                          (fun x->if x=ele then None else Some x) l2)
	            else false

(*fonction qui verifie si 2 environements sont egaux elle fait 
appel a la fonction precedente *)
let rec egal_env env1 env2 =
	match env1 with
	|[]->if env2=[] then true else false
	|ele::next->try if egal_list (snd ele) (List.assoc (fst ele) env2) 
	                 then egal_env next (List.remove_assoc (fst ele) env2)
			else false
		    with Not_found -> false

(* fonction qui prends une liste de signe et renvoie un string des signes *)    
let rec get_string_of_var signs=
	match signs with
	|[]->""
	|ele::next->(match ele with
		     |Pos->"+"^(get_string_of_var next)
		     |Neg->"-"^(get_string_of_var next)
		     |Zero->"0"^(get_string_of_var next)
		     |Error->"!"^(get_string_of_var next)
		     )

(* fonction qui prends un uplet de environement et string et appelle la 
fonction precedente pour chaque variables et concatène a la fin avec le 
string qui indique si ya une possibilé de division par 0 ou pas*)
let rec get_string_of_signs variables=
	match fst variables with
	|[]->if (snd variables)="" then "safe\n" else (snd variables)^"\n"
	|ele::next->(fst ele)^" "^(get_string_of_var (snd ele))^"\n"^(get_string_of_signs (next,snd variables))
               
