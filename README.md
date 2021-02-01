# projet-compilation

Compilation project - Polytech ET4 IT - Noted for S7 (2020)

powered by [![Ocaml](https://img.shields.io/badge/Ocaml-F29100?style=for-the-badge&logo=Ocaml&logoColor=white)](https://ocaml.org/)

## Asked work

- [Subject link](./doc/projet_2020-21.pdf) :fr:
- [Subject link - machine abstraite](./doc/ProjetCompilation.pdf) :fr:
- [Subject link- Code intermédiaire](./doc/CodeInt.pdf) :fr:

All remaining bugs and unimplemented features are listed [here](https://github.com/adepreis/projet-compilation/projects/1).

## How it works

* The `ast.ml` file which contains the ocaml type definitions for the Abstract Syntax Tree (AST).
* The `tpParse.mly` file is the parser designed for menhir.
* The `tpLex.mll` file contains the description of the lexical parser with ocamllex.
* The `testLex.ml` file allows you to see what the lexical analyser returns by printing the tokens it receives.
* The `Makefile` file is used to produce the different executables.

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
- [ocamllex](https://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html)


### Installation

Here are some instructions on how to get the development env running.

First, clone this repository with the following command :

`git clone https://github.com/adepreis/projet-compilation`

> ~~Then build the executable `interp` from source code using :~~
> 
> ~~`make` (or `make clean` to delete the executable file and all the object files from the directory)~~

Note : the `interp` file has already been created under `/interp` folder.

---

### Testing

You can find all the test files in the `/tests` repertory.

You can test the parser using

`make testLex` then `./testLex /tests/.../testFile.txt`


_When the code generation (PUSHI, STOREG, etc..) will work :_

> Use of the interpreter (at the project's root) :
> 
> `./interp/interp test-file-name`
> 
> with debug mode (step-by-step mode, setting breakpoints, visualize the memory content, the current instruction, etc..) :
> 
> `./interp/interp -d test-file-name`

---

### Contribution of each member of the group

- Lucas B. : grammar, parser, test files, lexical tester
- Antonin D. : grammar, parser, test files, lexical tester
- Rémy T. : grammar, AST, lexical analyser, conflicts resolution
- Tri-Man William V. : grammar, AST, lexical analyser, conflicts resolution, code generation


---

## Documentation

In the `/doc` folder, you can find some documentation about the project like the grammar we have created and later our project report.

## Debugging commands

Generate the `tpParse.conflicts` file that explains all the conflicts :

`menhir --lalr --explain tpParse.mly`

Generate the `tpParse.automaton` file that describes the automaton states :

`menhir --dump tpParse.mly`

<!--
### Schéma général / "étapes de construction du compilateur (??)"

`menhir [options] tpParse.mly`

`ocamllex [options] tpLex.mll`

`ocamlc [options] vos-fichiers.ml tpParse.mll tpLex.ml`
-->