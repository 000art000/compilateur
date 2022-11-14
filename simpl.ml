open Syntaxe 

(*fonction pour simplifie une expression *)
let rec simplifie_expr =function
 	|Num(c) as ret -> ret
 	|Var(v) as ret -> ret
 	|Op(op,ex1,ex2)->
 		let v1=simplifie_expr ex1 in let v2=simplifie_expr ex2 in
 		match op,v1,v2 with
 			|Add,Num(c1),Num(c2)->Num(c1+c2)
 			|Add,_,Num(0)->v1
 			|Add,Num(0),_->v2
 			|Sub,Num(c1),Num(c2)->Num(c1-c2)
 			|Sub,_,Num(0)->v1
 			|Mul,Num(0),_->Num(0)
 			|Mul,_,Num(0)->Num(0)
 			|Mul,Num(1),_->v2
 			|Mul,_,Num(1)->v1
 			|Mul,Num(c1),Num(c2)->Num(c1*c2)
 			|Div,Num(0),Num(0)->Op(op,v1,v2)
 			|Div,Num(c1),Num(c2)->Num(c1/c2)
 			|Mod,Num(0),Num(0)->Op(op,v1,v2)
 			|Mod,Num(c1),Num(c2)->Num(c1 mod c2)
                        |_,_,_->Op(op,v1,v2) 		

(*fonction qui simplifie une condition et retourne un Some boolean dans le cas 
ou on peut deduire le resultat de la condition sinon on retourne None *)                   
let simplifie_cond =function      
		|Num(c1),Eq,Num(c2)->Some(c1=c2)
		|Num(c1),Ne,Num(c2)->Some(c1!=c2)
		|Num(c1),Lt,Num(c2)->Some(c1<c2)
		|Num(c1),Le,Num(c2)->Some(c1<=c2)  
		|Num(c1),Gt,Num(c2)->Some(c1>c2)
		|Num(c1),Ge,Num(c2)->Some(c1>=c2) 
		|_,_,_->None              		
		
(*fonction qui simplifie le type program*)
let rec simplifie_pgm pgm =
     match pgm with
	|[]->[]
	|(p,Set(n,ex))::l->(* simplifie l'expression et la remetre*)
	let s=simplifie_expr ex in (p,Set(n,s))::simplifie_pgm l
	|((p,Read(v))as r)::l->r::simplifie_pgm l
	|(p,Print(ex))::l->(*simplifie l'expression et la remetre*)
	    let s=simplifie_expr ex in (p,Print(s))::simplifie_pgm l
	|(p,If(cond,if_b,else_b))::l->
	          (*simplifie les expressions de la condition et voir les cas 
	          si on entre toujour dans le if ou else ou on peut deduire*)
	          (match cond with
	           |v1,e,v2->
	              let h=(simplifie_expr v1,e,simplifie_expr v2) in
		      (match simplifie_cond h with
	              |Some (true)->(simplifie_pgm if_b)@(simplifie_pgm l)
	              |Some(false)->(simplifie_pgm else_b)@(simplifie_pgm l)
	              |_->(* remetre la condition simplifié avec 
	                   les block des if et else simplifiés  *)	             
	                  let b1=simplifie_pgm if_b in 
	                  let b2=simplifie_pgm else_b in   
                          (* verifier si les blocks sont vide ou pas des if et else *)	                
                     if b1=[] && b2=[] then (simplifie_pgm l)   
                     else if b1=[] && b2 <> [] then (b2 @ simplifie_pgm l)
                     else 	             
	             (p,If(h,b1,b2))::simplifie_pgm l)	              
		)
        |(p,While(cond,bloc1))::l->
        ( match cond with
	           |v1,e,v2->
	           (*simplifie les exression de la condition et voir les cas 
	           si on entre dans le while ou pas où on peut deduire*)
	        let h=(simplifie_expr v1,e,simplifie_expr v2) in
		(match simplifie_cond h with
	             |Some (true)->(p,While((Num(1),Eq,Num(1)),simplifie_pgm bloc1))
	                              ::(simplifie_pgm l)
	             |Some(false)->(simplifie_pgm l)
	             |_->
	             let b1=simplifie_pgm bloc1 in (p,While(h,b1))::simplifie_pgm l)	              
		)
	
	
