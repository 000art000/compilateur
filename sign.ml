open Syntaxe
open Eval 
open Sign_extra

(* fonction qui prend une lise des signe de l'expression 1 et l'operateur de 
l'expression  et la liste des signe de l'expression 2 et retoune une liste 
des signes possible pour l'operation *)
let rec sign_oper sign_exp1 sign_exp2 acc op=
   match sign_exp1 with
   |[]->acc
   |signe::next->(match op with
		| Add ->sign_oper next sign_exp2 
		          (union acc (sign_op_add signe sign_exp2 [])) op              
                | Sub ->sign_oper next sign_exp2 
                          (union acc (sign_op_sous signe sign_exp2 [])) op  
                | Mul ->sign_oper next sign_exp2 
                          (union acc (sign_op_mult signe sign_exp2 [])) op 
                | Div ->sign_oper next sign_exp2 
                          (union acc (sign_op_div signe sign_exp2 [])) op 
                | Mod ->sign_oper next sign_exp2 
                          (union acc (sign_op_mod signe sign_exp2 [])) op 
		)
	
(* fonction qui prend une expression et retourne les signes de l'expression
elle fait appel a la fonction precedente si l'expression et composé *)
let rec sign_exp expression variables =
	match expression with
	|Num(num)->if num>0 then [Pos] else if num<0 then [Neg] else [Zero]  
	|Var(nom)->(try List.assoc nom variables 
	            with Not_found ->(failwith ("erreur:"^nom^" n'existe pas")))
	|Op(operateur,expr1,expr2)->sign_oper (sign_exp expr1 variables) 
	                            (sign_exp expr2 variables) [] operateur

(* fonction qui prend en paramettre l'operateur de condition et 2 liste
de signes (liste de la 1ere et 2eme expression ) et verifie si la condition 
peut etre satisfait ou pas  *)	
let rec possibles op signe_exp1 signe_exp2 =
	match signe_exp1 with
	|[]->false 
	|ele::nxt->(match op with
		    | Eq ->if (possible_egal ele signe_exp2) then 
		    true else (possibles op nxt signe_exp2)
		    | Ne ->if (possible_negal ele signe_exp2) then true 
		         else possibles op nxt signe_exp2
		    | Lt ->if (possible_lt ele signe_exp2) then true 
		         else possibles op nxt signe_exp2
		    | Le ->if (possible_le ele signe_exp2) then true 
		         else possibles op nxt signe_exp2
		    | Gt ->if (possible_Gt ele signe_exp2) then true 
		         else possibles op nxt signe_exp2
		    | Ge ->if (possible_Ge ele signe_exp2) then true 
		         else possibles op nxt signe_exp2
		    )

(* fonction qui retourne une liste des signes qui satisfait la condition 
pour une variable donné (ele) *)			
let rec get_env_from_var cond ele liste variables acc lig safe=
    match liste with
    |[]->(acc,safe)
    |sign::nxt->(match cond with
	|ex1,op,ex2->
	   let signe_exp1=sign_exp ex1 ((ele,[sign])::variables)
	   in let signe_exp2=sign_exp ex2 ((ele,[sign])::variables)
	   in if(possibles op signe_exp1 signe_exp2)then
		if List.mem Error signe_exp1 || List.mem Error signe_exp2 then 
		    get_env_from_var cond ele nxt variables (sign::acc) lig 
		         ("divbyzero "^(string_of_int lig)) 
		else
		    get_env_from_var cond ele nxt variables (sign::acc) lig safe
	      else 
		if List.mem Error signe_exp1 || List.mem Error signe_exp2 then 
		    get_env_from_var cond ele nxt variables acc  lig 
		         ("divbyzero "^(string_of_int lig))
		else
		    get_env_from_var cond ele nxt variables acc lig safe
		)

(* fonction qui retourne pour chaqe variable dans la liste vars les signes qui 
satisfont la condition *)
let rec get_env_from_cond cond vars variables acc safe lig=
	match vars with
	|[]->(nouveau_env variables acc,safe)
	|ele::nxt->let liste=List.assoc ele variables  in 
	let signes,safe1=get_env_from_var cond ele liste 
	        (List.remove_assoc ele variables) [] lig ""
	 in if safe = "" then get_env_from_cond cond nxt variables 
	                      ((ele,signes)::acc) safe1 lig
	    else get_env_from_cond cond nxt variables 
	          ((ele,signes)::acc) safe lig
(* fonction qui prend en parametre *)
let sign_if_while cond variables safe lig= 
	let vars=get_variables_cond cond in 
             if vars=[] then
                 (
                 try
                    if(eval_cond cond [])then ([(true,variables);(false,[])],safe) 
                    else ([(false,[]);(true,variables)],safe) 
                 with _-> if safe ="" then ([(false,[]);(false,[])],"divbyzero "^
                                            (string_of_int lig)) 
                          else ([(false,[]);(false,[])],safe)  
                  ) 
             else(  
               let (nv_variables1,safe1)= 
                   get_env_from_cond cond vars variables [] safe lig in
               let (nv_variables2,_)= 
                   get_env_from_cond (invers_cond cond) vars variables [] safe lig in
               if safe = "" && safe1 <> "" then
                  if(List.assoc (List.hd vars) nv_variables1)=[]then (
                      if(List.assoc (List.hd vars) nv_variables2)=[]then 
                       ([(false,[]);(false,[])],safe1)
                      else ([(false,[]);(true,nv_variables2)],safe1)              
                  )
                  else(
                      if(List.assoc (List.hd vars) nv_variables2)=[]then 
                        ([(true,nv_variables1);(false,[])],safe1) 
                      else ([(true,nv_variables1);(true,nv_variables2)],safe1)  
                  )
               else 
                  if(List.assoc (List.hd vars) nv_variables1)=[]then (
                      if(List.assoc (List.hd vars) nv_variables2)=[]then 
                       ([(false,[]);(false,[])] ,safe)
                      else ([(false,[]);(true,nv_variables2)],safe)               
                  )
                  else(
                      if(List.assoc (List.hd vars) nv_variables2)=[]then 
                       ([(true,nv_variables1);(false,[])],safe) 
                      else ([(true,nv_variables1);(true,nv_variables2)],safe)   
                  )
              )

(* fonction qui retourne uplet de l'environement apres l'excution du while
 et string si il existe une devision par 0 ce environement *)	
let rec sign_while cond bloc1 variables safe lig=
	let resultat,safe=sign_if_while cond variables safe lig in
	if (fst (List.hd resultat)) then 
	let nv_vars=sign_bloc bloc1 (snd (List.hd resultat)) safe in
	(
	    if(fst (List.hd (List.tl resultat))) then 
		if egal_env variables (union_env variables (fst nv_vars)) then 
		      if safe = "" then (union_env 
		        (snd (List.hd (List.tl resultat))) variables,snd nv_vars)
		      else (union_env 
		        (snd (List.hd (List.tl resultat))) variables,safe)
		else 
		      if safe = "" then 
		          sign_while cond bloc1 (union_env variables (fst nv_vars))
		             (snd nv_vars) lig
		      else sign_while cond bloc1 (union_env variables (fst nv_vars)) 
		             safe lig
	    else 
		if egal_env variables (union_env variables (fst nv_vars)) then 
		   if safe = "" then 
		       (union_env (snd (List.hd (List.tl resultat))) variables
		       ,snd nv_vars)
		   else (union_env (snd (List.hd (List.tl resultat))) variables,safe)
		else 
		   if safe = "" then 
		        sign_while cond bloc1 (union_env variables (fst nv_vars)) 
		         (snd nv_vars) lig
		   else sign_while cond bloc1 (union_env variables (fst nv_vars)) 
		         safe lig
        )
	else let var=snd (List.hd (List.tl resultat)) in
	     if var = [] then (variables,safe)
	     else  (snd (List.hd (List.tl resultat)),safe)

(* fonction qui execute un block et retourne un uplet de l'environemt et  
string de si il devision par 0*)
and sign_bloc b variables safe= 
    (match b with
    |[]->(variables,safe)
    |ligne::b_suiv->let (variables,safe)=(sign_ligne ligne variables safe)in
                                   sign_bloc b_suiv variables safe)
(* fonction qui execute une ligne et retourne un uplet de l'environemt et  
string de si il devision par 0 *)
and sign_ligne ligne variables safe=
    match (snd ligne) with
    | Set(nom,express)->let sign=(sign_exp express variables) in 
                        let ret=(nom,sign)::(List.remove_assoc nom variables) 
                        in if List.mem Error sign && safe = "" then 
                        (ret,"divbyzero "^(string_of_int (fst ligne)))
                        else (ret,safe)
    | Read(nom)->((nom,[Pos;Neg;Zero])::(List.remove_assoc nom variables),safe)
    | Print(express)->let sign=(sign_exp express variables) in 
                        if List.mem Error sign && safe = "" then 
                        (variables,"divbyzero "^(string_of_int (fst ligne)))
                        else
                        (variables,safe) 
    | If(cond,bloc1,bloc2)->
                    let resultat,safe=sign_if_while cond variables safe 
                                      (fst ligne) in 
                    if(fst (List.hd resultat)) then 
                        let b1=(sign_bloc bloc1 (snd (List.hd resultat)) safe) in
    			if(fst (List.hd (List.tl resultat))) then 
    			     let b2=(sign_bloc bloc2 
    			            (snd (List.hd (List.tl resultat))) safe) in
    			     if safe="" then
    				if snd b1 <> "" then (union_env (fst b1) (fst b2),snd b1)
    				else (union_env (fst b1) (fst b2),snd b2)
    			     else (union_env (fst b1) (fst b2),safe)
    			else if safe="" && snd b1 <> "" then b1
    			     else (fst b1,safe)
    		     else 
    			if(fst (List.hd (List.tl resultat))) then 
    			    let b2=(sign_bloc bloc2 
    			           (snd (List.hd (List.tl resultat))) safe) in
    			    if safe="" && snd b2 <> "" then b2
    			    else (fst b2,safe)
    			else (variables,safe)
    | While(cond,bloc1)->sign_while cond bloc1 variables safe (fst ligne)
               
(* fonction qui affiche l'environement (pour chaque variables ces signes a 
la fin d'excution et si il existe une devision par 0 ou le program si il est safe) *)               
let affichage p=
    get_string_of_signs (sign_bloc p [] "")
    
