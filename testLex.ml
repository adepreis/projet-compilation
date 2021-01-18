open Lexing
open Ast
open TpParse


let output token =
    match token with
      CSTE v    -> "Constante entiere: " ^ (string_of_int v)
    | ID id     -> "Ident: " ^ id
    | IDC id    -> "Ident de classe: " ^ id
    | STRING s  -> "String: \"" ^ s ^ "\""
    | RELOP op  -> "operateur " ^ Misc.string_of_relop op
    | PLUS      -> "operateur +"
    | MINUS     -> "operateur -"
    | TIMES     -> "operateur *"
    | DIV       -> "operateur /"
    | CONCAT    -> "operateur &"
    | SEMICOLON -> "symbole ;"
    | LPAREN    -> "symbole ("
    | RPAREN    -> "symbole )"
    | ASSIGN    -> "affectation ="
    | IF        -> "mot-clef: IF"
    | THEN      -> "mot-clef: THEN"
    | ELSE      -> "mot-clef: ELSE"
    | CLASS     -> "mot-clef: CLASS"
    | EXTENDS   -> "mot-clef: EXTENDS"
    | IS        -> "mot-clef: IS"
    | VAR       -> "mot-clef: VAR"
    | DEF       -> "mot-clef: DEF"
    | OBJECT    -> "mot-clef: OBJECT"
    | OVERRIDE  -> "mot-clef: OVERRIDE"
    | THIS      -> "mot-clef: THIS"
    | SUPER     -> "mot-clef: SUPER"
    | RESULT    -> "mot-clef: RESULT"
    | NEW       -> "mot-clef: NEW"
    | AS        -> "mot-clef: AS"
    | RETURN    -> "mot-clef: RETURN"
    | DEUXPTS   -> "symbole :"
    | COMMA     -> "symbole ,"
    | DOT       -> "symbole ."
    | LACOLADE  -> "symbole {"
    | RACOLADE  -> "symbole }"
    | EOF       -> (* gere avant l'appel a cette fonction, donc impossible *)
                  failwith "Cannot happen"

(* usage: ./testLex nom-de-fichier
 * Applique l'analyseur lexical sur le fichier et imprime les tokens reconnus
 * (sauf ceux non transmis comme les delimiteurs et les commentaires)
 *)
let () =
  if Array.length Sys.argv = 1 then
    print_endline "usage: textLex nom-de-fichier"
  else
    begin
      let file = open_in Sys.argv.(1) in
      let lexbuf = Lexing.from_channel file
      in
      let rec process () =
        match TpLex.token lexbuf with
          EOF -> close_in file
        | tok -> print_endline (output tok); process ()
      in process ();
    end
;;
