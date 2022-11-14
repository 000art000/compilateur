open Syntaxe

(*retourner un string qui contient n espace*)
let rec string_of_indentation n acc=
    if n=0 then acc
    else  string_of_indentation (n-1) acc^" "

(*retourner un string de l'expression*)
let rec get_string_exp expression=
    match expression with
    | Num(num)->(string_of_int(num))^" "
    | Var(nom)->nom^" "
    | Op(operateur,expr1,expr2)->
                      let str=((get_string_exp expr1)^(get_string_exp expr2))in
                                  match operateur with
                                  | Add ->"+ "^str 
                                  | Sub ->"- "^str
                                  | Mul ->"* "^str
                                  | Div ->"/ "^str
                                  | Mod ->(String.make 1 '%')^" "^str

(* retourner un string de la condition *)
let get_string_cond condition=
    match condition with
    |(exp1,Eq,exp2)->(get_string_exp exp1)^"= "^(get_string_exp exp2)
    |(exp1,Ne,exp2)->(get_string_exp exp1)^"<> "^(get_string_exp exp2)
    |(exp1,Lt,exp2)->(get_string_exp exp1)^"< "^(get_string_exp exp2)
    |(exp1,Le,exp2)->(get_string_exp exp1)^"<= "^(get_string_exp exp2)
    |(exp1,Gt,exp2)->(get_string_exp exp1)^"> "^(get_string_exp exp2)
    |(exp1,Ge,exp2)->(get_string_exp exp1)^">= "^(get_string_exp exp2)

(* retourner un string sur le bloc *)
let rec get_string_bloc b indentation = 
    match b with
    |[]->""
    |ligne::b_suiv->(get_string_ligne ligne indentation)^
                     (get_string_bloc b_suiv indentation)
(* retourner un string sur l'instruction corespondente *)
and get_string_ligne ligne indentation=
    let s=string_of_indentation indentation "" in
    match (snd ligne) with
    | Set(nom,express)->s^nom^":= "^(get_string_exp express)^"\n";
    | Read(nom)->s^"READ "^nom^"\n"
    | Print(express)->s^"Print "^(get_string_exp express)^"\n"
    | If(condit,bloc1,bloc2)->
                 (let str=s^"IF "^(get_string_cond condit)^"\n"
                          ^(get_string_bloc bloc1 (indentation+2))
                  in if not (bloc2=[]) then 
                              str^s^"ELSE\n"^(get_string_bloc bloc2 (indentation+2))
                     else str)
    | While(condit,bloc1)->s^"WHILE "^(get_string_cond condit)^"\n"
                           ^(get_string_bloc bloc1 (indentation+2))
   
    
