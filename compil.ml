open Ast

let cptEti = ref 0;; (* compteur: variable globale *)

let makeEti () = (* generateur d'etiquettes fraiches *)
  let v = ! cptEti in
  let sv = string_of_int v in
  cptEti := v + 1;
  ("else"^sv, "fin"^sv)  (* ^ est l'operateur de concatenation de strings *)
;;

let compile ld e chan =
  (* compileDecl regarde les declarations de type ASSIGN selon moi... MAIS c'est parce que dans le TP, on a que des déclarations PUIS l'expression à évaluer *)
  let rec compileDecl ld env cpt =
    (* compile chaque partie droite de declaration et associe à sa partie
     * gauche son rang dans la liste des declarations, le rang correspondant
     * aussi au decalage de son emplacement par rapport à GP.
     *
     * cpt: rang pour la prochaine variable (initialisee a 0 au premier appel)
     * env :liste des couples (variable, rang de la variable ) deja traites
     * ld : liste des declarations a traiter
     *)
    match ld with
      [] -> env
    | { lhs; rhs; } :: ld' ->
       compileExpr rhs env;
       compileDecl ld' ((lhs, cpt)  :: env) (cpt+1)
  and compileExpr e env =
    match e with
      Id x ->
       begin
         (* retrouve le rang, donc l'adresse par rapport a GP, de la variable *)
         try let adr = List.assoc x env
             in output_string chan "PUSHG ";
                output_string chan (string_of_int adr);
                output_string chan "\n";
         with Not_found -> failwith ("variable non declaree: " ^ x)
       end
    | Cste v ->
       output_string chan "PUSHI "; output_string chan (string_of_int v);
       output_string chan "\n";
    | Plus(g, d) ->
       compileExpr g env; compileExpr d env; output_string chan "ADD\n";
    | Minus (g, d) ->
       compileExpr g env; compileExpr d env; output_string chan "SUB\n";
    | Times (g, d) ->
       compileExpr g env; compileExpr d env; output_string chan "MUL\n";
    | Div (g, d) ->
       compileExpr g env; compileExpr d env; output_string chan "DIV\n";
    | UMinus e -> (* traduit en 0 - e *)
       output_string chan "PUSHI 0\n";
       compileExpr e env;
       output_string chan "SUB\n"
    | Ite (si, alors, sinon) ->
       let (etiElse, etiFin) = makeEti () in
       compileExpr si env;
       output_string chan "JZ "; output_string chan etiElse;
       output_string chan "\n";
       compileExpr alors env;
       output_string chan "JUMP "; output_string chan etiFin;
       output_string chan "\n";
       output_string chan etiElse; output_string chan ": NOP\n";
       compileExpr sinon env;
       output_string chan etiFin; output_string chan ": NOP\n";
    | Comp(op, g, d) ->
       (* On montre l'autre facon de faire, par rapport a eval: on traite les
        * operateurs de comparaison ici, meme s'ils ne peuvent apparaitre
        * que dans la condtion d'un IF
        *)
       compileExpr g env; compileExpr d env;
       match op with
         Eq ->   output_string chan "EQUAL\n"
       | Neq ->  output_string chan "EQUAL\nNOT\n"
       | Lt ->   output_string chan "INF\n"
       | Le ->   output_string chan "INFEQ\n"
       | Gt ->   output_string chan "SUP\n"
       | Ge ->   output_string chan "SUPEQ\n"
  in
     output_string chan "START\n";
     compileExpr e (compileDecl ld [] 0);
     (* le resultat sera en sommet de pile. On imprime un message
      * puis le résultat, puis encore un saut de ligne et le STOP.
      * Attention à ce que les guillemets autour des chaines apparaissent bien
      * dans le code produit ainsi que le carcatere saut de ligne.
      *)
     output_string chan
       "PUSHS \"Resultat: \"\nWRITES\nWRITEI\nPUSHS \"\\n\"\nWRITES\nSTOP\n";
     flush chan;
     close_out chan;
;;


(* Intégrer cette fonction dans compile, puis l'appeler après l'appel de compileExpr, comme pour le truc du prof *)

let rec compileDefinition ld env cpt =
    (* compile une definition
     *
     * cpt: rang pour la prochaine variable (initialisee a 0 au premier appel)
     * env :liste des couples (variable, rang de la variable ) deja traites
     * ld : liste des definitions a traiter
     *)
    match ld with
      [] -> env

    | DefObj(x, lOptDecl) :: ld' ->
       compileDeclarationObjet lOptDecl env;

       (* compileExpr rhs env;
       compileDecl ld' ((lhs, cpt)  :: env) (cpt+1) *)

       compileDefinition ld' ((x, cpt) :: env) (cpt+1)

    | DefClasse(x, lParamClasse, optExtends, blockClasse) :: ld' ->
        compileDeclarationClasse blockClasse env;
        match optExtends with
          None -> compileDefinition ld' ((x,lParamClasse, optExtends, cpt) :: env) (cpt+1)
          |Some of  -> 
        
        
        


  and compileExpr e env =
    match e with
      Id x ->
       begin
         (* retrouve le rang, donc l'adresse par rapport a GP, de la variable *)
         try let adr = List.assoc x env
             in output_string chan "PUSHG ";
                output_string chan (string_of_int adr);
                output_string chan "\n";
         with Not_found -> failwith ("variable non declaree: " ^ x)
       end
    | Cste v ->
       output_string chan "PUSHI "; output_string chan (string_of_int v);
       output_string chan "\n";
    | Plus(g, d) ->
       compileExpr g env; compileExpr d env; output_string chan "ADD\n";
    | Minus (g, d) ->
       compileExpr g env; compileExpr d env; output_string chan "SUB\n";
    | Times (g, d) ->
       compileExpr g env; compileExpr d env; output_string chan "MUL\n";
    | Div (g, d) ->
       compileExpr g env; compileExpr d env; output_string chan "DIV\n";
    | UMinus e -> (* traduit en 0 - e *)
       output_string chan "PUSHI 0\n";
       compileExpr e env;
       output_string chan "SUB\n"
    | Ite (si, alors, sinon) ->
       let (etiElse, etiFin) = makeEti () in
       compileExpr si env;
       output_string chan "JZ "; output_string chan etiElse;
       output_string chan "\n";
       compileExpr alors env;
       output_string chan "JUMP "; output_string chan etiFin;
       output_string chan "\n";
       output_string chan etiElse; output_string chan ": NOP\n";
       compileExpr sinon env;
       output_string chan etiFin; output_string chan ": NOP\n";
    | Comp(op, g, d) ->
       (* On montre l'autre facon de faire, par rapport a eval: on traite les
        * operateurs de comparaison ici, meme s'ils ne peuvent apparaitre
        * que dans la condtion d'un IF
        *)
       compileExpr g env; compileExpr d env;
       match op with
         Eq ->   output_string chan "EQUAL\n"
       | Neq ->  output_string chan "EQUAL\nNOT\n"
       | Lt ->   output_string chan "INF\n"
       | Le ->   output_string chan "INFEQ\n"
       | Gt ->   output_string chan "SUP\n"
       | Ge ->   output_string chan "SUPEQ\n"
  in

