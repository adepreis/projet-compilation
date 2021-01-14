# projet-compilation

Compilation project - Polytech ET4 IT - Noted for S7 (2020)

## Objectives / Asked work

- [Subject link](./projet_2020-21.pdf) :fr:
- [Subject link - machine abstraite](./ProjetCompilation.pdf) :fr:
- [Subject link- Code intermédiaire](./CodeInt.pdf) :fr:

All remaining bugs and unimplemented features are listed [here](https://github.com/adepreis/projet-compilation/issues).

## How it works

* Un fichier `ast.ml` qui contient des définitions de type ocaml pour vos futurs Abstract Syntax Tree (AST).
* le fichier `tpParse.mly` contient un squelette de fichier pour menhir. Ce fichier est à compléter pour obtenir un analyseur syntaxique complet et correct, cohérent avec l'analyseur lexical.
* Le fichier `tpLex.mll` qui contient une ébauche de description pour construire votre futur analyseur lexical avec ocamllex. 
* le fichier `test_lex.ml` pour vous aider à visualiser ce que renvoie l’analyseur lexical: il se contente d'appeler l'analyseur lexical et d'imprimer des messages selon les tokens qu'il reçoit. Utile pour tester votre propre analyseur lexical.
* le fichier `main.ml` lance l'analyse syntaxique et, ultérieurement, les autres phases de la compilation.
* le fichier `Makefile` produit les différents exécutables

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development purpose.

### Prerequisites

Things you need to install the project :

- [ocaml](https://ocaml.org/)
- [opam](https://opam.ocaml.org/)
- [gcc](https://gcc.gnu.org/)
- [make](http://www.gnu.org/software/make/)
- [libfl-dev](https://packages.debian.org/fr/jessie/libfl-dev) (static library for flex)
- [bison](https://www.gnu.org/software/bison/)
- [menhir](http://gallium.inria.fr/~fpottier/menhir/)
- [ocamllex](https://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html) ??

### Schéma général / "étapes de construction du compilateur (??)"

`menhir [options] tpParse.mly`

`ocamllex [options] tpLex.mll`

`ocamlc [options] vos-fichiers.ml tpParse.mll tpLex.ml`


### "Installing"

Here are some instructions on how to get the development env running.

First, clone this repository with the following command :

`git clone https://github.com/adepreis/projet-compilation`

Then build the executable `interp` from source code using :

`make` (or `make clean` to delete the executable file and all the object files from the directory)


---

### Testing

You can find all the test files in the `/tests` repertory.

Use of the interpreter :

`interp test-file-name`

with debug mode (step-by-step mode, setting breakpoints, visualize the memory content, the current instruction, etc..) :

`interp -d test-file-name`

---

### Contribution of each member of the group

- Lucas B. : grammar, parser, test files
- Antonin D. : grammar, parser, test files
- Rémy T. : grammar, AST, lexical analyser
- Tri-Man William V. : grammar, AST, lexical analyser


---

## Documentation

In the `/doc` folder, you can find ...

## Debugging commands

Generate the `tpParse.conflicts` file that explains all the conflicts :

`menhir --lalr --explain tpParse.mly`

Generate the `tpParse.automaton` file that describes the automaton states :

`menhir --dump tpParse.mly`
