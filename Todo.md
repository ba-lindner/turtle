# TODOs

## General

* Multi-language support. As this tool is intended to be used for
  kids in school taking their first steps into programming who might
  not be too familiar with english, a german translation
  (not only of error messages, but also all the keywords of the language itself)
  would be rather helpful.
* Write a shift-reduce parser. Just for fun.

## Syntax checking

* Better Error Messages. Ideally, they should be easily
  machine-readable to allow VSCode to highlight errors in source code.
* Parsing after errors. As Keywords can be easily used to determine
  when the next statement starts, it should be easily possible to
  find multiple syntax errors at once (instead of currently stopping
  after the first one).

## Interpreter

Actually, nothing to do here.

## Debugger

* Implement Debug Adapter Protocol. This allows for a far better user interface
  (if you have VSCode and 2 screens).
* Ability to evaluate custom expressions or execute custom statements
* Ability to set variables

## Compiler

* Decide on what should actually be done as part of the compilation step
* Introduce autodiscovery of sdl (on *nix with `sdl-config`)
* maybe produce a makefile
* Allow for configuration of options passed to gcc

Currently, only compilation to C is implemented. An extension to other
languages, mostly Rust and Python, is targeted.