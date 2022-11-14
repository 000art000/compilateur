open Print
open Read
open Eval
open Vars
open Simpl
open Sign
open Syntaxe

let read_polish (filename:string) : program =
    read_file filename

let print_polish (p:program) : unit = 
    Printf.printf "%s" (get_string_bloc p 0)

let eval_polish (p:program) : unit = 
    let evaluation=eval_bloc p [] in ()

let vars_polish (p:program) : unit =
  Printf.printf "%s" (string_of_vars (vars_bloc p []) "" "")

let simpl_polish (p:program) : program=
    simplifie_pgm p  
    
let sing_polish (p:program) :unit =
    Printf.printf "%s" (affichage p)
  
let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> print_polish (read_polish file)
  | [|_;"--eval";file|] -> eval_polish (read_polish file)
  | [|_;"--vars";file|] -> vars_polish (read_polish file)
  | [|_;"--simpl";file|] -> print_polish (simpl_polish (read_polish file))
  | [|_;"--sign";file|] -> sing_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
