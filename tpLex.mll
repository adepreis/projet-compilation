{
open Ast
open TpParse
open Lexing
exception Eof

(* Gere les positions numero de ligne + decalage dans la ligne *)
let next_line lexbuf = Lexing.new_line lexbuf

(* Cree une table de hachage qu'on remplit avec le token associe a chaque mot-clef *)
let keyword_table = Hashtbl.create 16
let _ =
  List.iter
    (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ "if", IF;
      "then", THEN;
      "else", ELSE;

      "class", CLASS;
      "extends", EXTENDS;
      "is", IS;
      "var", VAR;
      "def", DEF;
      "object", OBJECT;
      "override", OVERRIDE;
      "this", THIS;
      "super", SUPER;
      "result", RESULT;
      "new", NEW;
      "as", AS;
      "return", RETURN
    ]
}

let lettre = ['A'-'Z' 'a'-'z']
let lettreMaj = ['A'-'Z']
(* Partie VII : "Les noms de classes et d’objets isolés doivent débuter par une majuscule ;
  tous les autres identificateurs doivent débuter par une minuscule.
  Les mots-clefs sont en minuscules" *)
let chiffre = ['0'-'9']
let LC = ( chiffre | lettre )


rule
 chaine tempo = parse
             "\"" { (* fin de string trouvee *)
                    STRING tempo
                  }
  | '\n'           { (* incremente le compteur de ligne et poursuit la
                      * reconnaissance de la chaine de caractère en cours
                      *)
                     new_line lexbuf; chaine tempo lexbuf
                   }
  | "\\\""           { (* detecte un echapement de quote dans une chaine *)
                     chaine (tempo ^ "\\\"") lexbuf
                   }
  | eof            { (* detecte les chaines de caractères non fermees pour
                      * pouvoir faire un message d'erreur clair *)
                     failwith "unclosed string";
                   }
  | _ as caractere { (* rien a faire de special pour ce caractere, donc on
                      * poursuit la reconnaissance du string en cours *)
                     chaine (tempo ^ (String.make 1 caractere)) lexbuf
                   }
and
 comment = parse
             "*/" { (* fin de commentaire trouvee. Le commentaire ne doit pas
                     * etre transmis. On renvoie donc ce que nous renverra un
                     * nouvel appel a l'analyseur lexical
                     *)
                    token lexbuf
                  }
  | '\n'           { (* incremente le compteur de ligne et poursuit la
                      * reconnaissance du commentaire en cours
                      *)
                     new_line lexbuf; comment lexbuf
                   }
  | eof            { (* detecte les commentaires non fermes pour pouvoir
                      * faire un message d'erreur clair.
                      * On pourrait stocker la position du dernier commentaire
                      * encore ouvert pour ameliorer le dioagnostic
                      *)
                     failwith "unclosed comment";
                   }
  | _              { (* rien a faire de special pour ce caractere, donc on
                      * poursuit la reconnaissance du commentaire en cours
                      *)
                     comment lexbuf
                   }
and
 token = parse
      lettre LC * as id
      {
      	(* id contient le texte reconnu. On verifie s'il s'agit d'un mot-clef
         * auquel cas on renvoie le token associe. Sinon on renvoie Id avec le
         * texte reconnu en valeur
         *)
        try
          Hashtbl.find keyword_table id
        with Not_found -> let i = int_of_char (String.get id 0) in
                    if (65 <= i && i <= 90)	(* Si la première lettre est une maj, on différencie ID et IDC *)
          				  then IDC id
          				  else ID id

      }
  | [' ''\t''\r']+  { (* consommer les delimiteurs, ne pas les transmettre
                       * et renvoyer ce que renverra un nouvel appel a
                       *  l'analyseur lexical
                       *)
                       token lexbuf
                    }
  | '\n'           { next_line lexbuf; token lexbuf}
  | chiffre+ as lxm { CSTE(int_of_string lxm) }
  | "/*"           { comment lexbuf }
  | "\""           { chaine "" lexbuf }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '&'            { CONCAT }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | ';'            { SEMICOLON }
  | '{'            { LACOLADE (* TOKENS AJOUTES *) }
  | '}'            { RACOLADE (* TOKENS AJOUTES *) }
  | ':'            { DEUXPTS (* TOKENS AJOUTES *) }
  | ','            { COMMA (* TOKENS AJOUTES *) }
  | '.'            { DOT (* TOKENS AJOUTES *) }
  | ":="           { ASSIGN }
  | "<"		         { RELOP (Ast.Lt) }
  | "<="           { RELOP (Ast.Le) }
  | ">"            { RELOP (Ast.Gt) }
  | ">="           { RELOP (Ast.Ge) }
  | "="            { RELOP (Ast.Eq) }
  | "<>"           { RELOP (Ast.Neq) }
  | eof            { EOF }
  | _ as lxm       { (* action par défaut: filtre un unique caractere, different
                      * de ceux qui precedent. Il s'agit d'un caratere errone:
                      * on le signale et on poursuit quand meme l'analyse
                      *)
                      print_endline
                       ("undefined character: " ^ (String.make 1 lxm));
                     token lexbuf
                   }
