# TODOs

- [ ] Implement features
  - [x] Types
    - [ ] Arrays
  - [x] Multithreading
  - [x] Events
    - [ ] events w/o types or feature dependency
  - [ ] Parameters
  - [ ] Early Returns
- [ ] improved debugger
  - [x] give feedback on what happened
  - [x] identify & choose active turtle
  - [ ] set variables
    - [ ] identify "const" calculations
    - [ ] allow these to be used
  - [x] view & edit breakpoints
    - [x] enable / disable
    - [x] add / remove
  - [ ] allow custom i/o for debug commands / output 
- [ ] Turtle Webserver
  - [x] add `Window` trait
  - [ ] implement buffered Window
  - [ ] implement API Window
  - [ ] write Client
- [ ] Compiler rewrite
  - [ ] return errors for unsupported features
  - [ ] actually support some features
  - [ ] add ability to choose language
  - [ ] better output configuration

## General

* Multi-language support. As this tool is intended to be used for
  kids in school taking their first steps into programming who might
  not be too familiar with english, a german translation
  (not only of error messages, but also all the keywords of
  the language itself) would be rather helpful.
* Additional checks for potential bugs. As they aren't hard errors,
  the checks should be optional and prompt on console if execution should
  be stopped or this position should be ignored on future occurences.

  Included Bugs:
  - empty loops: `do <expr> times ...` with negative `<expr>`
  - infinite loops:
    `counter <var> from <start> [to | downto] <end> [step <step>] ...`
    with unusual values of `<end>` / `<step>` and `to`/`downto`

## Syntax checking

* check number of arguments for function calls
* Better Error Messages. Ideally, they should be easily
  machine-readable to allow VSCode to highlight errors in source code.
* Parsing after errors. As Keywords can be easily used to determine
  when the next statement starts, it should be easily possible to
  find multiple syntax errors at once (instead of currently stopping
  after the first one).

## Interpreter

Actually, nothing to do here.

Well, you can look at Extensions.md for some ideas.

## Debugger

* Implement Debug Adapter Protocol. This allows for a far better
  user interface (if you have VSCode and 2 screens).
* Ability to evaluate custom expressions or execute custom statements
* Ability to set variables

## Compiler

* use `#ifdef` for debug printf statements in turtleinterf.c
  instead of turtleinterf_debug.c
* Decide on what should actually be done as part of the compilation step
* Introduce autodiscovery of sdl (on *nix with `sdl-config`)
* maybe produce a makefile
* Allow for configuration of options passed to gcc

Currently, only compilation to C is implemented. An extension to other
languages, mostly Rust and Python, is targeted.